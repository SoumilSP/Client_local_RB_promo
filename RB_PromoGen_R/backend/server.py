from fastapi import FastAPI, UploadFile, File, Form, HTTPException, Request
from starlette.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from typing import Dict, Optional
import httpx
import logging
import os
import shutil
import json
from pathlib import Path

import subprocess
import asyncio
import time

from config import settings
from routes import promo

# Create the main app
app = FastAPI(title="PromoGen API", version="2.0.0")

# R API URL (Plumber running on port 8002)
R_API_URL = os.getenv("R_API_URL", "http://localhost:8002")


@app.on_event("startup")
async def startup_event():
    logger.info("PromoGen API starting up...")
    logger.info(f"Data directory: {DATA_DIR}")
    logger.info(f"R API URL: {R_API_URL}")


def unbox_r_value(val):
    """
    Handle R's jsonlite serialization quirks.
    R serializes single-value vectors as single-element arrays [123.45] instead of scalars 123.45.
    This function extracts the value from single-element arrays.
    """
    if isinstance(val, list):
        if len(val) == 1:
            return val[0]
        elif len(val) == 0:
            return None
        else:
            return val
    return val


def unbox_r_dict(d) -> dict:
    """
    Recursively unbox all values in a dictionary that came from R.
    """
    if not isinstance(d, dict):
        return d
    result = {}
    for key, val in d.items():
        if isinstance(val, dict):
            result[key] = unbox_r_dict(val)
        elif isinstance(val, list):
            if len(val) == 1 and not isinstance(val[0], dict):
                # Single element array of non-dict - unbox it
                result[key] = val[0]
            elif len(val) == 1 and isinstance(val[0], dict):
                # Single element array of dict - unbox and recurse
                result[key] = unbox_r_dict(val[0])
            else:
                # Multi-element array - recursively process each element
                result[key] = [unbox_r_dict(item) if isinstance(item, dict) else item for item in val]
        else:
            result[key] = val
    return result


def unbox_r_response(data):
    """Unbox any R response data"""
    if isinstance(data, dict):
        return unbox_r_dict(data)
    elif isinstance(data, list):
        return [unbox_r_dict(item) if isinstance(item, dict) else unbox_r_value(item) for item in data]
    return data

# Get the backend directory (where server.py is located)
BACKEND_DIR = Path(__file__).parent.resolve()

# Data directory for uploaded files - use environment variable or relative path
DATA_DIR = Path(os.getenv("DATA_DIR", BACKEND_DIR.parent / "data"))
DATA_DIR.mkdir(exist_ok=True)

# File mapping config path
FILE_MAPPING_PATH = DATA_DIR / "file_mapping.json"

# Include promo router (mock data for now)
app.include_router(promo.router, prefix="/api")

# CORS
cors_origins = settings.CORS_ORIGINS
if cors_origins == "*":
    app.add_middleware(
        CORSMiddleware,
        allow_credentials=True,
        allow_origins=["*"],
        allow_methods=["*"],
        allow_headers=["*"],
    )
else:
    app.add_middleware(
        CORSMiddleware,
        allow_credentials=True,
        allow_origins=cors_origins.split(','),
        allow_methods=["*"],
        allow_headers=["*"],
    )

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


@app.get("/api/")
async def root():
    return {"message": "PromoGen API", "version": "2.0.0", "engine": "R + FastAPI"}


@app.get("/api/health")
async def health_check():
    # Check R API health
    r_status = "unknown"
    try:
        async with httpx.AsyncClient() as client:
            resp = await client.get(f"{R_API_URL}/health", timeout=5.0)
            if resp.status_code == 200:
                r_status = "healthy"
    except Exception:
        r_status = "unavailable"
    
    return {
        "status": "healthy",
        "r_engine": r_status,
        "data_dir": str(DATA_DIR)
    }


# ==========================================
# AUTHENTICATION ENDPOINT
# Simple hardcoded credential check
# ==========================================
class LoginRequest(BaseModel):
    username: str
    password: str


@app.post("/api/auth/login")
async def login(request: LoginRequest):
    """
    Simple hardcoded login endpoint.
    Credentials are stored on the backend, not exposed to frontend.
    """
    import hashlib
    import secrets
    
    # Hardcoded credentials - stored only on backend
    VALID_USERNAME = "Reckitt_admin"
    VALID_PASSWORD = "RB_Internal@Login#01"
    
    # Validate credentials
    if request.username == VALID_USERNAME and request.password == VALID_PASSWORD:
        # Generate a simple session token
        token = secrets.token_hex(32)
        logger.info(f"User '{request.username}' logged in successfully")
        return {
            "success": True,
            "token": token,
            "message": "Login successful"
        }
    else:
        logger.warning(f"Failed login attempt for username: '{request.username}'")
        raise HTTPException(
            status_code=401,
            detail="Invalid username or password"
        )


@app.post("/api/data/upload")
async def upload_data_file(
    file: UploadFile = File(...),
    file_id: Optional[str] = Form(None)
):
    """Upload Excel/CSV data file for analysis"""
    # Validate file type
    allowed_extensions = {'.xlsx', '.xls', '.csv'}
    file_ext = Path(file.filename).suffix.lower()
    
    if file_ext not in allowed_extensions:
        raise HTTPException(
            status_code=400,
            detail=f"Invalid file type. Allowed: {', '.join(allowed_extensions)}"
        )
    
    # Save file
    file_path = DATA_DIR / file.filename
    try:
        # Delete existing file first if it exists (handles Windows file locking)
        if file_path.exists():
            try:
                file_path.unlink()
            except PermissionError:
                # File might be locked, try with a different name
                import time
                file_path = DATA_DIR / f"{Path(file.filename).stem}_{int(time.time())}{file_ext}"
        
        # Read file content first, then write
        contents = await file.read()
        with open(file_path, "wb") as buffer:
            buffer.write(contents)
        
        logger.info(f"File uploaded: {file.filename} (id: {file_id})")
        
        # Update file mapping if file_id provided
        if file_id:
            mapping = {}
            if FILE_MAPPING_PATH.exists():
                with open(FILE_MAPPING_PATH, 'r') as f:
                    mapping = json.load(f)
            mapping[file_id] = str(file_path)
            with open(FILE_MAPPING_PATH, 'w') as f:
                json.dump(mapping, f, indent=2)
        
        return {
            "success": True,
            "filename": file.filename,
            "path": str(file_path),
            "size": file_path.stat().st_size,
            "file_id": file_id,
            "message": "File uploaded successfully. Ready for processing."
        }
    except Exception as e:
        logger.error(f"Upload error: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/api/data/files")
async def list_data_files():
    """List all uploaded data files"""
    files = []
    for f in DATA_DIR.iterdir():
        if f.is_file() and f.suffix.lower() in {'.xlsx', '.xls', '.csv'}:
            files.append({
                "name": f.name,
                "size": f.stat().st_size,
                "modified": f.stat().st_mtime
            })
    
    # Also return the mapping if it exists
    mapping = {}
    if FILE_MAPPING_PATH.exists():
        with open(FILE_MAPPING_PATH, 'r') as f:
            mapping = json.load(f)
    
    return {"files": files, "mapping": mapping}


@app.delete("/api/data/files/{filename}")
async def delete_data_file(filename: str):
    """Delete a data file"""
    file_path = DATA_DIR / filename
    if not file_path.exists():
        raise HTTPException(status_code=404, detail="File not found")
    
    file_path.unlink()
    return {"success": True, "message": f"File {filename} deleted"}


class ProcessRequest(BaseModel):
    file_mapping: Dict[str, str]
    retailer: Optional[str] = "Carrefour"       # SP_cust
    manufacturer: Optional[str] = "Reckitt"     # SP_manuf
    brand: Optional[str] = "DETTOL"             # SP_brand
    level: Optional[str] = "PPG"                # SP_level


@app.post("/api/data/process")
async def process_data_files(request: ProcessRequest):
    """Process uploaded data files through R engine"""
    file_mapping = request.file_mapping
    
    logger.info(f"Processing files: {list(file_mapping.keys())}")
    logger.info(f"Config: retailer={request.retailer}, manufacturer={request.manufacturer}, brand={request.brand}, level={request.level}")
    
    # Save file mapping with forward slashes for R compatibility
    file_mapping_for_save = {}
    for file_id, file_path in file_mapping.items():
        # Convert backslashes to forward slashes for R
        file_mapping_for_save[file_id] = str(file_path).replace("\\", "/")
    
    with open(FILE_MAPPING_PATH, 'w') as f:
        json.dump(file_mapping_for_save, f, indent=2)
    
    logger.info(f"Saved file mapping to {FILE_MAPPING_PATH}")
    
    # Verify all files exist
    missing_files = []
    for file_id, file_path in file_mapping.items():
        if not Path(file_path).exists():
            missing_files.append(file_id)
    
    if missing_files:
        return {
            "success": False,
            "error": f"Missing files: {', '.join(missing_files)}"
        }
    
    # Call R API to process files - use query parameters, not JSON body
    try:
        async with httpx.AsyncClient() as client:
            # R endpoint expects query params: retailer, manufacturer, brand
            resp = await client.post(
                f"{R_API_URL}/data/process",
                params={
                    "retailer": request.retailer,
                    "manufacturer": request.manufacturer, 
                    "brand": request.brand
                },
                timeout=300.0  # 5 minutes for processing
            )
            
            if resp.status_code == 200:
                result = resp.json()
                logger.info(f"R processing result: {result}")
                return {
                    "success": result.get("success", [True])[0] if isinstance(result.get("success"), list) else result.get("success", True),
                    "message": "Data processed successfully",
                    "data_prep_created": True,
                    "details": result
                }
            else:
                return {
                    "success": False,
                    "error": f"R API error: {resp.text}"
                }
    except httpx.ConnectError:
        # R API not available
        logger.warning("R API not available, files saved but not processed")
        return {
            "success": False,
            "message": "R Engine not running. Please start R Engine first.",
            "r_status": "unavailable"
        }
    except Exception as e:
        logger.error(f"Processing error: {e}")
        return {
            "success": False,
            "error": str(e)
        }


@app.get("/api/data/mapping")
async def get_file_mapping():
    """Get current file mapping configuration"""
    if FILE_MAPPING_PATH.exists():
        with open(FILE_MAPPING_PATH, 'r') as f:
            return {"mapping": json.load(f)}
    return {"mapping": {}}


@app.get("/api/data/summary")
async def get_data_summary():
    """Get summary of all loaded data from R engine"""
    try:
        async with httpx.AsyncClient() as client:
            resp = await client.get(f"{R_API_URL}/analytics/data-summary", timeout=30.0)
            return resp.json()
    except httpx.ConnectError:
        return {"error": "R API unavailable", "files_configured": 0}
    except Exception as e:
        return {"error": str(e)}


@app.get("/api/data/rdata-info")
async def get_rdata_info():
    """Get information about the processed data_prep_op.RData file"""
    import os
    from datetime import datetime
    
    rdata_path = DATA_DIR / "data_prep_op.RData"
    
    if not rdata_path.exists():
        return {
            "exists": False,
            "message": "No processed data file found. Please run data processing first."
        }
    
    # Get file stats
    stat = rdata_path.stat()
    file_size = stat.st_size
    created_time = datetime.fromtimestamp(stat.st_mtime)
    
    # Format size
    if file_size < 1024:
        size_str = f"{file_size} B"
    elif file_size < 1024 * 1024:
        size_str = f"{file_size / 1024:.1f} KB"
    else:
        size_str = f"{file_size / (1024 * 1024):.2f} MB"
    
    # Try to get component info from R
    components = []
    try:
        async with httpx.AsyncClient() as client:
            resp = await client.get(f"{R_API_URL}/data/rdata-components", timeout=30.0)
            if resp.status_code == 200:
                data = resp.json()
                components = data.get("components", [])
    except Exception as e:
        logger.warning(f"Could not get RData components from R: {e}")
    
    return {
        "exists": True,
        "filename": "data_prep_op.RData",
        "path": str(rdata_path),
        "size": file_size,
        "size_formatted": size_str,
        "created": created_time.isoformat(),
        "created_formatted": created_time.strftime("%Y-%m-%d %H:%M:%S"),
        "components": components
    }


@app.get("/api/data/rdata-preview/{component_index}")
async def get_rdata_preview(component_index: int, rows: int = 10):
    """Get a preview of a specific component from data_prep_op.RData"""
    try:
        async with httpx.AsyncClient() as client:
            resp = await client.get(
                f"{R_API_URL}/data/rdata-preview",
                params={"index": component_index, "rows": rows},
                timeout=30.0
            )
            if resp.status_code == 200:
                return unbox_r_response(resp.json())
            else:
                return {"error": f"R API returned status {resp.status_code}"}
    except httpx.ConnectError:
        return {"error": "R API unavailable"}
    except Exception as e:
        return {"error": str(e)}


@app.get("/api/data/nielsen")
async def get_nielsen_data():
    """Get Nielsen RMS data summary"""
    try:
        async with httpx.AsyncClient() as client:
            resp = await client.get(f"{R_API_URL}/analytics/executive-summary", timeout=30.0)
            return resp.json()
    except httpx.ConnectError:
        return {"error": "R API unavailable"}
    except Exception as e:
        return {"error": str(e)}


@app.get("/api/data/events")
async def get_events_data():
    """Get events/promotions data"""
    try:
        async with httpx.AsyncClient() as client:
            resp = await client.get(f"{R_API_URL}/analytics/events", timeout=30.0)
            return resp.json()
    except httpx.ConnectError:
        return {"error": "R API unavailable"}
    except Exception as e:
        return {"error": str(e)}


@app.get("/api/data/model-results")
async def get_model_results_data():
    """Get model results data"""
    try:
        async with httpx.AsyncClient() as client:
            resp = await client.get(f"{R_API_URL}/analytics/model-results", timeout=30.0)
            return resp.json()
    except httpx.ConnectError:
        return {"error": "R API unavailable"}
    except Exception as e:
        return {"error": str(e)}


# ==========================================
# PRODUCT DEFAULTS ENDPOINT WITH PYTHON FALLBACK
# Reference: data_prep_event_list.R lines 1052-1080
# LY_Investment = sum(Trade_Investment) for promo weeks
# Min_Investment = 0.1 * LY_Investment (10%)
# Max_Investment = 10 * LY_Investment (1000%)
# ==========================================
@app.get("/api/r/optimizer/product-defaults")
async def get_product_defaults(ppg: str = ""):
    """
    Get calculated default product restrictions for a PPG.
    Tries R API first, falls back to Python calculation if R is unavailable.
    """
    logger.info(f"GET /api/r/optimizer/product-defaults - PPG: {ppg}")
    
    # Try R API first
    try:
        async with httpx.AsyncClient() as client:
            resp = await client.get(
                f"{R_API_URL}/optimizer/product-defaults",
                params={"ppg": ppg},
                timeout=30.0
            )
            if resp.status_code == 200:
                return unbox_r_response(resp.json())
    except httpx.ConnectError:
        logger.info("R API unavailable, using Python fallback for product defaults")
    except Exception as e:
        logger.warning(f"R API error, using Python fallback: {e}")
    
    # Python fallback: Calculate investment defaults from CSV files
    try:
        import pandas as pd
        
        # Define multiple possible paths for data files
        R_ENGINE_DIR = BACKEND_DIR / "r_engine"
        
        # Try to load optimizer data CSV from multiple locations
        optimizer_csv_paths = [
            DATA_DIR / "shiny_ip_optimizer.csv",
            R_ENGINE_DIR / "shiny_ip_optimizer.csv"
        ]
        events_csv_paths = [
            DATA_DIR / "shiny_ip_event.csv",
            R_ENGINE_DIR / "shiny_ip_event.csv"
        ]
        prod_restrictions_csv_paths = [
            DATA_DIR / "shiny_ip_prod_restrictions.csv",
            R_ENGINE_DIR / "shiny_ip_prod_restrictions.csv"
        ]
        
        # Helper to find first existing path
        def find_file(paths):
            for p in paths:
                if p.exists():
                    return p
            return None
        
        optimizer_csv_path = find_file(optimizer_csv_paths)
        events_csv_path = find_file(events_csv_paths)
        prod_restrictions_csv_path = find_file(prod_restrictions_csv_paths)
        
        logger.info("Looking for data files:")
        logger.info(f"  - events: {events_csv_path}")
        logger.info(f"  - optimizer: {optimizer_csv_path}")
        logger.info(f"  - prod_restrictions: {prod_restrictions_csv_path}")
        
        # Determine data source
        trade_data = None
        data_source = None
        
        # Try events file first (has Trade_Investment)
        if events_csv_path and events_csv_path.exists():
            try:
                trade_data = pd.read_csv(events_csv_path)
                if 'Trade_Investment' in trade_data.columns and 'PPG' in trade_data.columns:
                    data_source = "events"
                    logger.info(f"Loaded events data: {len(trade_data)} rows")
            except Exception as e:
                logger.warning(f"Failed to load events CSV: {e}")
        
        # Fallback to optimizer file
        if trade_data is None and optimizer_csv_path and optimizer_csv_path.exists():
            try:
                trade_data = pd.read_csv(optimizer_csv_path)
                if 'Trade_Investment' in trade_data.columns and 'PPG' in trade_data.columns:
                    data_source = "optimizer"
                    logger.info(f"Loaded optimizer data: {len(trade_data)} rows")
            except Exception as e:
                logger.warning(f"Failed to load optimizer CSV: {e}")
        
        # Load prod_restrictions for RSP_Unit and pre-calculated values
        prod_restrictions = None
        if prod_restrictions_csv_path and prod_restrictions_csv_path.exists():
            try:
                prod_restrictions = pd.read_csv(prod_restrictions_csv_path)
                logger.info(f"Loaded prod_restrictions: {len(prod_restrictions)} rows, columns: {list(prod_restrictions.columns)}")
            except Exception as e:
                logger.warning(f"Failed to load prod_restrictions CSV: {e}")
        
        # Determine PPGs to process
        ppg_list = []
        if trade_data is not None and 'PPG' in trade_data.columns:
            ppg_list = trade_data['PPG'].unique().tolist()
        elif prod_restrictions is not None and 'PPG' in prod_restrictions.columns:
            ppg_list = prod_restrictions['PPG'].unique().tolist()
        
        if not ppg_list:
            return {"success": False, "error": "No PPG data available"}
        
        # Filter to requested PPG if specified
        if ppg and ppg != "ALL" and ppg in ppg_list:
            ppg_list = [ppg]
        
        results = {}
        for current_ppg in ppg_list:
            ppg_defaults = {
                "ppg": current_ppg,
                "rsp_unit": None,
                "price_min": None,
                "price_max": None,
                "lsm_price_min": None,
                "lsm_price_max": None,
                "ly_investment": None,
                "min_investment": None,
                "max_investment": None,
                "min_slots": 1,
                "max_slots": 6,
                "source": "python_fallback"
            }
            
            # Calculate LY_Investment from Trade_Investment
            # Reference: data_prep_event_list.R lines 1052-1053
            if trade_data is not None and 'Trade_Investment' in trade_data.columns:
                ppg_rows = trade_data[trade_data['PPG'] == current_ppg]
                if len(ppg_rows) > 0:
                    ly_investment = ppg_rows['Trade_Investment'].sum()
                    if pd.notna(ly_investment) and ly_investment > 0:
                        ppg_defaults["ly_investment"] = round(ly_investment, 0)
                        # Reference: data_prep_event_list.R lines 1079-1080
                        ppg_defaults["min_investment"] = round(0.1 * ly_investment, 0)
                        ppg_defaults["max_investment"] = round(10 * ly_investment, 0)
                        ppg_defaults["source"] = f"calculated_from_{data_source}"
                        logger.info(f"PPG {current_ppg}: LY_Investment={ly_investment}, Min={ppg_defaults['min_investment']}, Max={ppg_defaults['max_investment']}")
            
            # Get RSP_Unit and price constraints from prod_restrictions
            if prod_restrictions is not None and 'PPG' in prod_restrictions.columns:
                ppg_row = prod_restrictions[prod_restrictions['PPG'] == current_ppg]
                if len(ppg_row) > 0:
                    row = ppg_row.iloc[0]
                    
                    # RSP_Unit
                    for col in ['RSP_Unit', 'RSP', 'RSP (unit)']:
                        if col in row.index and pd.notna(row[col]):
                            ppg_defaults["rsp_unit"] = float(row[col])
                            break
                    
                    # Price constraints
                    if 'Non_LSM_Min_Promo_Price' in row.index and pd.notna(row['Non_LSM_Min_Promo_Price']):
                        ppg_defaults["price_min"] = float(row['Non_LSM_Min_Promo_Price'])
                    if 'Non_LSM_Max_Promo_Price' in row.index and pd.notna(row['Non_LSM_Max_Promo_Price']):
                        ppg_defaults["price_max"] = float(row['Non_LSM_Max_Promo_Price'])
                    if 'LSM_Promo_Price_Min' in row.index and pd.notna(row['LSM_Promo_Price_Min']):
                        ppg_defaults["lsm_price_min"] = float(row['LSM_Promo_Price_Min'])
                    if 'LSM_Promo_Price_Max' in row.index and pd.notna(row['LSM_Promo_Price_Max']):
                        ppg_defaults["lsm_price_max"] = float(row['LSM_Promo_Price_Max'])
                    
                    # LY_Investment from prod_restrictions (calculated in data_prep)
                    if 'LY_Investment' in row.index and pd.notna(row['LY_Investment']):
                        ppg_defaults["ly_investment"] = float(row['LY_Investment'])
                        # Also recalculate min/max from LY_Investment to ensure consistency
                        # Reference: data_prep_event_list.R lines 1079-1080
                        ppg_defaults["min_investment"] = round(0.1 * float(row['LY_Investment']), 0)
                        ppg_defaults["max_investment"] = round(10 * float(row['LY_Investment']), 0)
                        ppg_defaults["source"] = "calculated_from_ly_investment"
                        logger.info(f"PPG {current_ppg}: Recalculated from LY_Investment={ppg_defaults['ly_investment']}")
                    # Use prod_restrictions investment values ONLY if not calculated from LY_Investment
                    elif ppg_defaults["min_investment"] is None:
                        if 'Min_Investment' in row.index and pd.notna(row['Min_Investment']):
                            ppg_defaults["min_investment"] = float(row['Min_Investment'])
                        else:
                            ppg_defaults["min_investment"] = 50000  # Fallback
                    if ppg_defaults["max_investment"] is None:
                        if 'Max_Investment' in row.index and pd.notna(row['Max_Investment']):
                            ppg_defaults["max_investment"] = float(row['Max_Investment'])
                        else:
                            ppg_defaults["max_investment"] = 200000  # Fallback
            
            # Calculate price from RSP if not available
            if ppg_defaults["rsp_unit"] and ppg_defaults["rsp_unit"] > 0:
                rsp = ppg_defaults["rsp_unit"]
                if ppg_defaults["price_min"] is None:
                    ppg_defaults["price_min"] = round(0.33 * rsp, 2)
                if ppg_defaults["price_max"] is None:
                    ppg_defaults["price_max"] = round(0.9 * rsp, 2)
                if ppg_defaults["lsm_price_max"] is None:
                    ppg_defaults["lsm_price_max"] = round(rsp, 2)
            
            # Final fallback for investment if still None
            if ppg_defaults["min_investment"] is None:
                ppg_defaults["min_investment"] = 50000
            if ppg_defaults["max_investment"] is None:
                ppg_defaults["max_investment"] = 200000
            
            results[current_ppg] = ppg_defaults
        
        # Return single PPG data if specific PPG was requested
        if ppg and ppg != "ALL" and len(results) == 1:
            return {
                "success": True,
                "ppg": ppg,
                "defaults": results[ppg],
                "source": "python_fallback"
            }
        
        return {
            "success": True,
            "ppg_count": len(results),
            "defaults": results,
            "source": "python_fallback"
        }
        
    except Exception as e:
        logger.error(f"Error calculating product defaults: {e}")
        # Ultimate fallback with static defaults
        return {
            "success": True,
            "defaults": {
                "ppg": ppg or "default",
                "rsp_unit": None,
                "price_min": 3.49,
                "price_max": 4.49,
                "lsm_price_min": None,
                "lsm_price_max": None,
                "ly_investment": None,
                "min_investment": 50000,
                "max_investment": 200000,
                "min_slots": 1,
                "max_slots": 6,
                "source": "static_fallback"
            },
            "source": "static_fallback",
            "error": str(e)
        }


# ==========================================
# ALTERNATE EVENTS ENDPOINT WITH PYTHON FALLBACK
# Reads from shiny_ip_event.csv when R is unavailable
# ==========================================
@app.get("/api/r/optimizer/alternate-events")
async def get_alternate_events(ppg: str = "", tesco_week_no: str = ""):
    """
    Get alternate events for a slot from shiny_ip_events data.
    Tries R API first, falls back to Python CSV parsing if R is unavailable.
    """
    logger.info(f"GET /api/r/optimizer/alternate-events - PPG: {ppg}, Week: {tesco_week_no}")
    
    # Try R API first
    try:
        async with httpx.AsyncClient() as client:
            resp = await client.get(
                f"{R_API_URL}/optimizer/alternate-events",
                params={"ppg": ppg, "tesco_week_no": tesco_week_no},
                timeout=30.0
            )
            if resp.status_code == 200:
                return unbox_r_response(resp.json())
    except httpx.ConnectError:
        logger.info("R API unavailable, using Python fallback for alternate events")
    except Exception as e:
        logger.warning(f"R API error for alternate events, using Python fallback: {e}")
    
    # Python fallback: Read from shiny_ip_event.csv
    try:
        import pandas as pd
        
        R_ENGINE_DIR = BACKEND_DIR / "r_engine"
        
        # Find the events CSV file
        events_csv_paths = [
            R_ENGINE_DIR / "shiny_ip_event.csv",
            DATA_DIR / "shiny_ip_event.csv",
            R_ENGINE_DIR / "11 Display Events.csv",
        ]
        
        events_df = None
        for csv_path in events_csv_paths:
            if csv_path.exists():
                try:
                    events_df = pd.read_csv(csv_path)
                    logger.info(f"Loaded events from: {csv_path} ({len(events_df)} rows)")
                    break
                except Exception as e:
                    logger.warning(f"Failed to read {csv_path}: {e}")
        
        if events_df is None or events_df.empty:
            return {"error": "No events data available", "alternates": [], "source": "python_fallback"}
        
        # Log columns for debugging
        logger.info(f"Events CSV columns: {events_df.columns.tolist()[:15]}")
        
        # Filter by PPG
        ppg_col = 'PPG' if 'PPG' in events_df.columns else 'ppg'
        ppg_events = events_df[events_df[ppg_col] == ppg].copy()
        logger.info(f"Events for PPG {ppg}: {len(ppg_events)} rows")
        
        if ppg_events.empty:
            return {"ppg": ppg, "alternates": [], "source": "python_fallback", "message": "No events for this PPG"}
        
        # Get price constraints from stored optimization params
        price_min = 0
        price_max = 999
        
        # Try to get from prod_restrictions CSV
        prod_restrictions_path = R_ENGINE_DIR / "prod_restrictions.csv"
        if prod_restrictions_path.exists():
            try:
                prod_df = pd.read_csv(prod_restrictions_path)
                ppg_row = prod_df[prod_df['PPG'] == ppg]
                if not ppg_row.empty:
                    price_min = float(ppg_row['price_min'].iloc[0]) if 'price_min' in ppg_row.columns else 0
                    price_max = float(ppg_row['price_max'].iloc[0]) if 'price_max' in ppg_row.columns else 999
                    logger.info(f"Price constraints from prod_restrictions: {price_min} - {price_max}")
            except Exception as e:
                logger.warning(f"Failed to read price constraints: {e}")
        
        # Apply price filter if Promo_Price column exists
        price_cols = [c for c in events_df.columns if 'promo' in c.lower() and 'price' in c.lower()]
        if price_cols:
            price_col = price_cols[0]
            # Calculate Promo_Price from Discount and RSP if needed
            if price_col in ppg_events.columns:
                ppg_events = ppg_events[
                    (ppg_events[price_col] >= price_min) & 
                    (ppg_events[price_col] <= price_max)
                ]
                logger.info(f"After price filter ({price_min}-{price_max}): {len(ppg_events)} rows")
        
        # Build alternates list
        alternates = []
        
        for idx, row in ppg_events.head(50).iterrows():  # Limit to 50
            # Determine mechanic
            display_flag = int(row.get('Display_Flag', row.get('display_flag', 0)) or 0)
            mechanic = "Display" if display_flag == 1 else "TPR"
            
            # Get display type
            display_type = str(row.get('Display', row.get('display', '')))
            if pd.isna(display_type) or display_type.strip() in ['', 'nan', ' ']:
                display_type = ""
            
            # Get discount (stored as decimal in CSV, convert to percentage)
            discount = float(row.get('Discount', row.get('discount', 0)) or 0) * 100
            
            # Get RSP for promo price calculation
            rsp = float(row.get('RSP_Unit', row.get('RSP (unit)', row.get('RSP', 13.51))) or 13.51)
            promo_price = rsp * (1 - discount / 100)
            
            # Get ROI values
            roi = float(row.get('ROI_GM', row.get('R_ROI_GM', row.get('ROI', 0))) or 0)
            
            # Get KPI values (these might be calculated columns)
            gs = float(row.get('Gross_Sales', row.get('GS', 0)) or 0)
            nr = float(row.get('Net_Revenue', row.get('NR', 0)) or 0)
            ts = float(row.get('Total_Trade_Investment', row.get('TS', 0)) or 0)
            gm = float(row.get('GM_Abs', row.get('GM', 0)) or 0)
            
            alternates.append({
                "id": len(alternates) + 1,
                "mechanic": mechanic,
                "displayType": display_type,
                "promoPrice": round(promo_price, 2),
                "discount": round(discount, 0),
                "rsp": round(rsp, 2),
                "roi": round(roi, 2),
                "GS": round(gs, 0),
                "NR": round(nr, 0),
                "TS": round(ts, 0),
                "GM": round(gm, 0)
            })
        
        # Sort by ROI descending
        alternates.sort(key=lambda x: x.get('roi', 0), reverse=True)
        
        logger.info(f"Returning {len(alternates)} alternates from Python fallback")
        if alternates:
            logger.info(f"Sample alternate: {alternates[0]}")
        
        return {
            "ppg": ppg,
            "tesco_week_no": tesco_week_no,
            "price_constraints": {"min": price_min, "max": price_max},
            "total_events_for_ppg": len(ppg_events),
            "alternates": alternates,
            "source": "python_fallback"
        }
    
    except Exception as e:
        logger.error(f"Python fallback error for alternate events: {e}")
        import traceback
        logger.error(traceback.format_exc())
        return {"error": str(e), "alternates": [], "source": "python_fallback_error"}


# Proxy endpoints to R API
@app.get("/api/r/{path:path}")
async def proxy_r_get(path: str):
    """Proxy GET requests to R API with R array unboxing"""
    try:
        async with httpx.AsyncClient() as client:
            resp = await client.get(f"{R_API_URL}/{path}", timeout=60.0)
            # Unbox R's single-element arrays
            return unbox_r_response(resp.json())
    except httpx.ConnectError:
        raise HTTPException(status_code=503, detail="R API unavailable")
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/api/r/{path:path}")
async def proxy_r_post(path: str, request: Request, body: dict = None):
    """Proxy POST requests to R API with R array unboxing"""
    try:
        # Get query parameters and convert to dict for R
        query_params = dict(request.query_params)
        
        # Special handling for optimizer/run endpoint - extract parameters from JSON body
        if path == "optimizer/run" and body:
            logger.info("=" * 60)
            logger.info("OPTIMIZER API CALL TRACE")
            logger.info("=" * 60)
            logger.info(f"[1] Frontend called: POST /api/r/{path}")
            logger.info(f"[2] Body keys received: {list(body.keys())}")
            
            # Extract all sections from nested structure
            settings = body.get('settings', {})
            constraints = body.get('constraints', [])
            # Handle restrictions being either a list or dict
            restrictions_raw = body.get('restrictions', {})
            restrictions = restrictions_raw if isinstance(restrictions_raw, dict) else {}
            filters = body.get('filters', {})
            
            # Log ALL received parameters
            logger.info("[3] FILTERS received:")
            logger.info(f"    - retailer: {filters.get('retailer')}")
            logger.info(f"    - brand: {filters.get('brand')}")
            logger.info(f"    - ppg: {filters.get('ppg')}")
            logger.info(f"    - dateType: {filters.get('dateType')}")
            logger.info(f"    - startMonth: {filters.get('startMonth')}")
            logger.info(f"    - endMonth: {filters.get('endMonth')}")
            logger.info(f"    - startQuarter: {filters.get('startQuarter')}")
            logger.info(f"    - endQuarter: {filters.get('endQuarter')}")
            
            logger.info("[4] SETTINGS received:")
            logger.info(f"    - goal: {settings.get('goal')}")
            logger.info(f"    - sign: {settings.get('sign')}")
            logger.info(f"    - slotCriterion: {settings.get('slotCriterion')}")
            logger.info(f"    - runType: {settings.get('runType')}")
            logger.info(f"    - roiType: {settings.get('roiType')}")
            
            logger.info(f"[5] CONSTRAINTS received: {constraints}")
            
            logger.info("[6] RESTRICTIONS received:")
            logger.info(f"    - priceMin: {restrictions.get('priceMin')}")
            logger.info(f"    - priceMax: {restrictions.get('priceMax')}")
            logger.info(f"    - slotsMin: {restrictions.get('slotsMin')}")
            logger.info(f"    - slotsMax: {restrictions.get('slotsMax')}")
            logger.info(f"    - displayMin: {restrictions.get('displayMin')}")
            logger.info(f"    - displayMax: {restrictions.get('displayMax')}")
            logger.info(f"    - flyerMin: {restrictions.get('flyerMin')}")
            logger.info(f"    - flyerMax: {restrictions.get('flyerMax')}")
            logger.info(f"    - minInvestment: {restrictions.get('minInvestment')}")
            logger.info(f"    - maxInvestment: {restrictions.get('maxInvestment')}")
            logger.info(f"    - mechanics: {restrictions.get('mechanics')}")
            
            # Map R Shiny goal names to internal codes
            goal_mapping = {
                'Scan Net Revenue': 'NR',
                'Gross Margin % of NR': 'GM%NR',
                'Volume Sales': 'VOL',
                'Scan Gross Sales': 'GS',
                'Incremental GM ROI': 'GM_ROI',
                'Trade Spend % of NR': 'TS%NR',
                'Value Market Share': 'VMS',
                'Trade Spend % of NIS': 'TS%NIS',
                'Gross Margin': 'GM',
            }
            
            # Get the goal from settings and map it
            goal_input = settings.get('goal', 'Scan Net Revenue')
            goal_code = goal_mapping.get(goal_input, goal_input)  # Use mapping or original if not found
            logger.info(f"    - goal mapped: {goal_input} -> {goal_code}")
            
            # Build comprehensive query params for R Plumber
            r_params = {
                # Settings - including new runType and roiType
                'goal': goal_code,
                'goal_full_name': goal_input,  # Send full name too for R to use
                'goal_sign': settings.get('sign', 'Max'),
                'slot_criterion': settings.get('slotCriterion', 'Seasonality Trend'),
                'run_type': settings.get('runType', 'Run Complete Optimization'),
                'roi_type': settings.get('roiType', 'Incremental GM ROI'),
                
                # Filters
                'retailer': filters.get('retailer', 'Carrefour'),
                'brand': filters.get('brand', 'DETTOL'),
                'ppg': filters.get('ppg', ''),
                'date_type': filters.get('dateType', 'monthly'),
                'start_quarter': filters.get('startQuarter', 'Q1'),
                'end_quarter': filters.get('endQuarter', 'Q4'),
            }
            
            # CRITICAL: Resolve start_month/end_month based on date_type
            # When dateType is 'quarterly', the monthly fields may hold stale values
            # We must derive actual months from quarters to ensure correct date range
            QUARTER_START = {'Q1': 'Jan', 'Q2': 'Apr', 'Q3': 'Jul', 'Q4': 'Oct'}
            QUARTER_END = {'Q1': 'Mar', 'Q2': 'Jun', 'Q3': 'Sep', 'Q4': 'Dec'}
            
            if filters.get('dateType') == 'quarterly':
                start_q = filters.get('startQuarter', 'Q1')
                end_q = filters.get('endQuarter', 'Q4')
                r_params['start_month'] = QUARTER_START.get(start_q, 'Jan')
                r_params['end_month'] = QUARTER_END.get(end_q, 'Dec')
                logger.info(f"    - Quarterly mode: {start_q}-{end_q} -> {r_params['start_month']}-{r_params['end_month']}")
            else:
                r_params['start_month'] = filters.get('startMonth', 'Jan')
                r_params['end_month'] = filters.get('endMonth', 'Dec')
            
            r_params.update({
                # Restrictions - Price
                'price_min': restrictions.get('priceMin', 3.49),
                'price_max': restrictions.get('priceMax', 4.49),
                
                # Restrictions - Slots
                'slots_min': restrictions.get('slotsMin', 1),
                'slots_max': restrictions.get('slotsMax', 6),
                
                # Restrictions - Display mechanic constraints
                'display_min': restrictions.get('displayMin', 0),
                'display_max': restrictions.get('displayMax', 10),
                
                # Restrictions - Flyer mechanic constraints (future use)
                'flyer_min': restrictions.get('flyerMin', 0),
                'flyer_max': restrictions.get('flyerMax', 10),
                
                # Mechanics as comma-separated string
                'mechanics': ','.join([
                    k for k, v in restrictions.get('mechanics', {}).items() if v
                ]) if restrictions.get('mechanics') else 'display,flyer,displayFlyer',
            })
            
            # Extract constraint values from the constraints list/dict
            # R Shiny sends a table with KPIs: "Scan Net Revenue", "Gross Margin % of NR", "Trade Spend % of NR", 
            # "Trade Spend % of NIS", "Scan Gross Sales", "Gross Margin", "Volume Sales", "Incremental GM ROI", "Value Market Share"
            # Each KPI maps to a specific internal code used by optimization()
            
            # IMPORTANT: The R Shiny UI displays "Absolute" values in MILLIONS (value / 10^6)
            # But the optimizer compares against ACTUAL raw values
            # So we need to multiply "Absolute" scale values by 10^6 to convert back
            
            # KPI to internal code mapping (from R Shiny server.R line 2947)
            KPI_MAPPING = {
                'scan net revenue': 'nr',           # Net_Sales_model (Absolute - display in millions)
                'gross margin % of nr': 'gm',       # GM_percent_model (Percent)
                'trade spend % of nr': 'ts_nr',     # Trade_as_per_NR_model (Percent)
                'trade spend % of nis': 'ts_nis',   # Trade_as_per_NIS_model (Percent)
                'scan gross sales': 'gs',           # Gross_sales_model (Absolute - display in millions)
                'gross margin': 'gm_abs',           # Gross_margin_model (Absolute - display in millions)
                'volume sales': 'vol',              # Volume_sales_model (Absolute - display in millions)
                'incremental gm roi': 'roi_gm',     # ROI_model (GM variant)
                'incremental nr roi': 'roi_nr',     # ROI_model (NR variant)
                'incremental nis roi': 'roi_nis',   # ROI_model (NIS variant)
                'value market share': 'ms',         # Market_Share_model (Percent)
            }
            
            # KPIs that use "Absolute" scale and need to be multiplied by 10^6
            ABSOLUTE_SCALE_KPIS = {'nr', 'gs', 'gm_abs', 'vol'}
            
            # Initialize all constraint parameters
            all_constraints = {}
            
            if isinstance(constraints, list):
                for constraint in constraints:
                    kpi = constraint.get('kpi', '').lower().strip()
                    min_val = constraint.get('min', '')
                    max_val = constraint.get('max', '')
                    scale = constraint.get('scale', 'Absolute')
                    
                    # Find the internal code for this KPI
                    internal_code = KPI_MAPPING.get(kpi)
                    
                    if internal_code:
                        # Convert min/max to floats, applying scale factor for "Absolute" KPIs
                        # Absolute scale KPIs are displayed in millions, so multiply by 10^6
                        scale_factor = 1e6 if internal_code in ABSOLUTE_SCALE_KPIS else 1
                        
                        converted_min = ''
                        converted_max = ''
                        
                        if min_val != '' and min_val is not None:
                            converted_min = float(min_val) * scale_factor
                        if max_val != '' and max_val is not None:
                            converted_max = float(max_val) * scale_factor
                        
                        all_constraints[internal_code] = {
                            'min': converted_min,
                            'max': converted_max,
                            'scale': scale,
                            'kpi': kpi,
                            'original_min': min_val,
                            'original_max': max_val
                        }
                        logger.info(f"[7a] Constraint [{internal_code}] {kpi}: display={min_val}-{max_val}, converted={converted_min}-{converted_max}, scale={scale}, factor={scale_factor}")
                    else:
                        logger.warning(f"[7a] Unknown KPI: {kpi}")
                
            elif isinstance(constraints, dict):
                # Direct dict format with keys like gmMin, gmMax, etc.
                # These are assumed to be already in actual values (not scaled)
                all_constraints['gm'] = {'min': constraints.get('gmMin', 28), 'max': constraints.get('gmMax', 35)}
                all_constraints['ts_nr'] = {'min': constraints.get('tsMin', 50000), 'max': constraints.get('tsMax', 200000)}
                all_constraints['nr'] = {'min': constraints.get('nrMin', 100000), 'max': constraints.get('nrMax', 500000)}
            
            # Map to the simplified r_params for backward compatibility with plumber_api.R
            # IMPORTANT: Following R Shiny server.R lines 2938-2942:
            # - If a constraint is NOT included, Min = -10^20 and Max = 10^20 (effectively unconstrained)
            # - This matches the client's exact behavior
            
            # Large values to represent "unconstrained" (matches R Shiny behavior)
            UNCONSTRAINED_MIN = -1e20
            UNCONSTRAINED_MAX = 1e20
            
            # GM% constraint (Gross Margin % of NR) - Percent scale, no conversion needed
            if 'gm' in all_constraints:
                c = all_constraints['gm']
                r_params['gm_min'] = float(c['min']) if c['min'] != '' else UNCONSTRAINED_MIN
                r_params['gm_max'] = float(c['max']) if c['max'] != '' else UNCONSTRAINED_MAX
            else:
                # GM not passed - use unconstrained values
                r_params['gm_min'] = UNCONSTRAINED_MIN
                r_params['gm_max'] = UNCONSTRAINED_MAX
            
            # Trade Spend % of NR constraint - Percent scale, no conversion needed
            if 'ts_nr' in all_constraints:
                c = all_constraints['ts_nr']
                r_params['ts_min'] = float(c['min']) if c['min'] != '' else UNCONSTRAINED_MIN
                r_params['ts_max'] = float(c['max']) if c['max'] != '' else UNCONSTRAINED_MAX
            else:
                # Trade Spend % of NR not passed - use unconstrained values
                r_params['ts_min'] = UNCONSTRAINED_MIN
                r_params['ts_max'] = UNCONSTRAINED_MAX
            
            # Trade Spend % of NIS constraint - Percent scale
            if 'ts_nis' in all_constraints:
                c = all_constraints['ts_nis']
                r_params['ts_nis_min'] = float(c['min']) if c['min'] != '' else UNCONSTRAINED_MIN
                r_params['ts_nis_max'] = float(c['max']) if c['max'] != '' else UNCONSTRAINED_MAX
            else:
                # Trade Spend % of NIS not passed - use unconstrained values  
                r_params['ts_nis_min'] = UNCONSTRAINED_MIN
                r_params['ts_nis_max'] = UNCONSTRAINED_MAX
            
            # Net Revenue constraint (Scan Net Revenue) - Absolute scale, already converted
            if 'nr' in all_constraints:
                c = all_constraints['nr']
                r_params['nr_min'] = float(c['min']) if c['min'] != '' else UNCONSTRAINED_MIN
                r_params['nr_max'] = float(c['max']) if c['max'] != '' else UNCONSTRAINED_MAX
            else:
                # Net Revenue not passed - use unconstrained values
                r_params['nr_min'] = UNCONSTRAINED_MIN
                r_params['nr_max'] = UNCONSTRAINED_MAX
            
            # Gross Sales constraint (Scan Gross Sales) - Absolute scale
            if 'gs' in all_constraints:
                c = all_constraints['gs']
                r_params['gs_min'] = float(c['min']) if c['min'] != '' else UNCONSTRAINED_MIN
                r_params['gs_max'] = float(c['max']) if c['max'] != '' else UNCONSTRAINED_MAX
            else:
                r_params['gs_min'] = UNCONSTRAINED_MIN
                r_params['gs_max'] = UNCONSTRAINED_MAX
            
            # Volume Sales constraint - Absolute scale
            if 'vol' in all_constraints:
                c = all_constraints['vol']
                r_params['vol_min'] = float(c['min']) if c['min'] != '' else UNCONSTRAINED_MIN
                r_params['vol_max'] = float(c['max']) if c['max'] != '' else UNCONSTRAINED_MAX
            else:
                r_params['vol_min'] = UNCONSTRAINED_MIN
                r_params['vol_max'] = UNCONSTRAINED_MAX
            
            # ROI constraints (any variant: roi_gm, roi_nr, roi_nis)
            roi_found = False
            for roi_key in ['roi_gm', 'roi_nr', 'roi_nis']:
                if roi_key in all_constraints:
                    c = all_constraints[roi_key]
                    r_params['roi_min'] = float(c['min']) if c['min'] != '' else UNCONSTRAINED_MIN
                    r_params['roi_max'] = float(c['max']) if c['max'] != '' else UNCONSTRAINED_MAX
                    roi_found = True
                    break
            if not roi_found:
                r_params['roi_min'] = UNCONSTRAINED_MIN
                r_params['roi_max'] = UNCONSTRAINED_MAX
            
            # Gross Margin (Absolute) constraint - Absolute scale, already converted by 1e6
            if 'gm_abs' in all_constraints:
                c = all_constraints['gm_abs']
                r_params['gm_abs_min'] = float(c['min']) if c['min'] != '' else UNCONSTRAINED_MIN
                r_params['gm_abs_max'] = float(c['max']) if c['max'] != '' else UNCONSTRAINED_MAX
            else:
                r_params['gm_abs_min'] = UNCONSTRAINED_MIN
                r_params['gm_abs_max'] = UNCONSTRAINED_MAX
            
            # Value Market Share constraint - Percent scale
            if 'ms' in all_constraints:
                c = all_constraints['ms']
                r_params['ms_min'] = float(c['min']) if c['min'] != '' else UNCONSTRAINED_MIN
                r_params['ms_max'] = float(c['max']) if c['max'] != '' else UNCONSTRAINED_MAX
            else:
                r_params['ms_min'] = UNCONSTRAINED_MIN
                r_params['ms_max'] = UNCONSTRAINED_MAX
            
            # Investment constraints from Product Restrictions (Min/Max Investment)
            # These match R Shiny's Product Restrictions table columns: "Min Investment", "Max Investment"
            # Values are in AED (absolute currency)
            min_investment = restrictions.get('minInvestment', 50000)
            max_investment = restrictions.get('maxInvestment', 200000)
            
            # Validate and set investment constraints
            r_params['min_investment'] = float(min_investment) if min_investment else 0
            r_params['max_investment'] = float(max_investment) if max_investment else 10000000
            
            logger.info(f"[7b] Investment constraints: min={r_params['min_investment']}, max={r_params['max_investment']}")
            
            # Pass all constraints as JSON for the R backend to use properly
            import json
            r_params['constraints_json'] = json.dumps(all_constraints)
            
            # Ensure price constraints are properly typed
            r_params['price_min'] = float(r_params.get('price_min', 3.49))
            r_params['price_max'] = float(r_params.get('price_max', 4.49))
            
            logger.info(f"[7] Final R query params: gm={r_params.get('gm_min')}-{r_params.get('gm_max')}, ts_nr={r_params.get('ts_min')}-{r_params.get('ts_max')}, ts_nis={r_params.get('ts_nis_min')}-{r_params.get('ts_nis_max')}, nr={r_params.get('nr_min')}-{r_params.get('nr_max')}")
            logger.info(f"[7a] GS={r_params.get('gs_min')}-{r_params.get('gs_max')}, Vol={r_params.get('vol_min')}-{r_params.get('vol_max')}, GM_Abs={r_params.get('gm_abs_min')}-{r_params.get('gm_abs_max')}, MS={r_params.get('ms_min')}-{r_params.get('ms_max')}, ROI={r_params.get('roi_min')}-{r_params.get('roi_max')}")
            logger.info(f"[7b] Price constraints: min={r_params.get('price_min')}, max={r_params.get('price_max')}")
            logger.info(f"[7c] Slot constraints: min={r_params.get('slots_min')}, max={r_params.get('slots_max')}")
            logger.info(f"[7d] Investment constraints: min={r_params.get('min_investment')}, max={r_params.get('max_investment')}")
            logger.info(f"[8] Calling R Plumber API: POST http://localhost:8002/{path}")
            logger.info("=" * 60)
            
            async with httpx.AsyncClient() as client:
                resp = await client.post(f"{R_API_URL}/{path}", params=r_params, timeout=300.0)
                logger.info(f"[9] R API Response status: {resp.status_code}")
                return unbox_r_response(resp.json())
        
        async with httpx.AsyncClient() as client:
            # If query params exist, append them to URL for R Plumber
            if query_params:
                r_url = f"{R_API_URL}/{path}"
                resp = await client.post(r_url, params=query_params, timeout=300.0)
            else:
                resp = await client.post(f"{R_API_URL}/{path}", json=body or {}, timeout=300.0)
            # Unbox R's single-element arrays
            return unbox_r_response(resp.json())
    except httpx.ConnectError:
        raise HTTPException(status_code=503, detail="R API unavailable")
    except Exception as e:
        logger.error(f"R proxy error: {e}")
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/api/r-engine/status")
async def get_r_engine_status():
    """Get detailed R engine status"""
    try:
        async with httpx.AsyncClient() as client:
            resp = await client.get(f"{R_API_URL}/health", timeout=5.0)
            if resp.status_code == 200:
                return {
                    "status": "connected",
                    "message": "R Engine is running",
                    "details": resp.json() if resp.text else {}
                }
            else:
                return {
                    "status": "error",
                    "message": f"R Engine returned status {resp.status_code}"
                }
    except httpx.ConnectError:
        return {
            "status": "disconnected",
            "message": "R Engine is not running"
        }
    except Exception as e:
        return {
            "status": "error",
            "message": str(e)
        }


@app.post("/api/r-engine/start")
async def start_r_engine():
    """Start the R Plumber API engine"""
    
    # Check if already running
    try:
        async with httpx.AsyncClient() as client:
            resp = await client.get(f"{R_API_URL}/health", timeout=3.0)
            if resp.status_code == 200:
                return {
                    "success": True,
                    "message": "R Engine is already running",
                    "status": "connected"
                }
    except Exception:
        pass  # Not running, proceed to start
    
    try:
        # Start R Plumber API in background
        r_engine_path = BACKEND_DIR / "r_engine"
        
        # Platform-specific command
        import platform
        if platform.system() == "Windows":
            # Windows: use start /B for background
            cmd = f'cd /d "{r_engine_path}" && start /B Rscript start_api.R'
            subprocess.Popen(cmd, shell=True)
        else:
            # Linux/Mac: use nohup
            log_file = "/var/log/r_api.log"
            cmd = f"cd {r_engine_path} && nohup Rscript start_api.R > {log_file} 2>&1 &"
            subprocess.Popen(cmd, shell=True)
        
        logger.info("R Engine start command issued")
        
        # Wait a bit and check if it started
        import asyncio
        await asyncio.sleep(3)
        
        try:
            async with httpx.AsyncClient() as client:
                resp = await client.get(f"{R_API_URL}/health", timeout=5.0)
                if resp.status_code == 200:
                    return {
                        "success": True,
                        "message": "R Engine started successfully",
                        "status": "connected"
                    }
        except Exception:
            pass
        
        return {
            "success": True,
            "message": "R Engine start command issued. It may take a few seconds to initialize.",
            "status": "starting"
        }
        
    except Exception as e:
        logger.error(f"Failed to start R Engine: {e}")
        return {
            "success": False,
            "message": f"Failed to start R Engine: {str(e)}",
            "status": "error"
        }


@app.on_event("shutdown")
async def shutdown_event():
    logger.info("PromoGen API shutting down...")

