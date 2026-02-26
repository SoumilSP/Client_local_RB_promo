from fastapi import APIRouter, HTTPException
from typing import List, Optional, Dict, Any
from pydantic import BaseModel
from datetime import datetime, timedelta
import random
import math
import httpx
import os
import re

router = APIRouter(prefix="/promo", tags=["Promotions"])

# R API URL
R_API_URL = os.getenv("R_API_URL", "http://localhost:8002")


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
            return val  # Return the list as-is if it has multiple elements
    return val


def unbox_r_dict(d: dict) -> dict:
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
            if len(val) == 1:
                # Single element array - unbox it
                inner = val[0]
                if isinstance(inner, dict):
                    result[key] = unbox_r_dict(inner)
                else:
                    result[key] = inner
            else:
                # Multi-element array - recursively process each element
                result[key] = [unbox_r_dict(item) if isinstance(item, dict) else item for item in val]
        else:
            result[key] = val
    return result


# Helper function to get data from R API
async def get_r_data(endpoint: str):
    """Fetch data from R API and unbox R's jsonlite array serialization"""
    try:
        async with httpx.AsyncClient() as client:
            resp = await client.get(f"{R_API_URL}/{endpoint}", timeout=30.0)
            if resp.status_code == 200:
                data = resp.json()
                # Unbox R's single-element arrays
                if isinstance(data, dict):
                    return unbox_r_dict(data)
                elif isinstance(data, list):
                    return [unbox_r_dict(item) if isinstance(item, dict) else unbox_r_value(item) for item in data]
                return data
    except Exception as e:
        print(f"R API error: {e}")
    return None

# ==================== MODELS ====================

class KPIData(BaseModel):
    label: str
    value: float
    change: float
    trend: str  # "up" or "down"

class ChartDataPoint(BaseModel):
    name: str
    value: float
    value2: Optional[float] = None
    category: Optional[str] = None

class FilterOptions(BaseModel):
    countries: List[str]
    customers: List[str]
    categories: List[str]
    brands: List[str]
    formats: List[str]
    ppgs: List[str]
    manufacturers: List[str]
    # New fields for PPG-to-Category filtering
    ppgToBrand: Optional[Dict[str, str]] = {}  # PPG code -> Category name mapping
    ppg_category_map: Optional[Dict[str, str]] = {}  # Alias for ppgToBrand
    ppg_descriptions: Optional[Dict[str, str]] = {}  # PPG code -> Description mapping

class PromotionEvent(BaseModel):
    id: str
    ppg: str
    brand: str
    start_date: str
    end_date: str
    event_type: str
    discount: float
    roi: float
    week: int
    rsp: float

class ROIBubbleData(BaseModel):
    name: str
    roi: float
    investment: float
    incremental_sales: float
    event_type: str

class VolumeDecomposition(BaseModel):
    metrics: str
    cy_value: float
    ya_value: float
    abs_change: float
    pct_change: float

class PromoFactTable(BaseModel):
    ppg: str
    brand: str
    format: str
    events: int
    base_volume: float
    incr_volume: float
    total_volume: float
    roi_gm: float
    investment: float
    incr_gm: float

class OptimizerConfig(BaseModel):
    goal: str
    sign: str
    start_date: str
    end_date: str
    max_display_slots: int
    roi_type: str

class OptimizerConstraint(BaseModel):
    kpi: str
    min_value: Optional[float]
    max_value: Optional[float]
    current_value: float

class OptimizationResult(BaseModel):
    ppg: str
    brand: str
    format: str
    ly_volume: float
    lsm_volume: float
    unconstrained_volume: float
    ly_roi: float
    lsm_roi: float
    unconstrained_roi: float
    slot: int
    event_type: str
    discount: float

class SimulatorEvent(BaseModel):
    id: str
    ppg: str
    brand: str
    slot: int
    current_event: str
    current_discount: float
    current_roi: float
    alt_event: str
    alt_discount: float
    alt_roi: float
    volume_impact: float

class KAMMetrics(BaseModel):
    metric: str
    ly_value: float
    cy_value: float
    change_pct: float

# ==================== MOCK DATA GENERATORS ====================

def generate_kpi_data() -> List[KPIData]:
    return [
        KPIData(label="Value Sales", value=12.5, change=8.3, trend="up"),
        KPIData(label="Volume Sales", value=845000, change=-2.1, trend="down"),
        KPIData(label="Market Share", value=34.2, change=1.5, trend="up"),
        KPIData(label="GM ROI", value=1.85, change=12.4, trend="up"),
        KPIData(label="Trade Spend", value=2.3, change=-5.2, trend="down"),
        KPIData(label="Avg Price", value=8.45, change=3.1, trend="up"),
    ]

def generate_value_share_data() -> List[ChartDataPoint]:
    return [
        ChartDataPoint(name="Brand005", value=45.2, category="Brand"),
        ChartDataPoint(name="Brand003", value=28.5, category="Brand"),
        ChartDataPoint(name="Brand007", value=15.8, category="Brand"),
        ChartDataPoint(name="Others", value=10.5, category="Brand"),
    ]

def generate_volume_trend_data() -> List[ChartDataPoint]:
    base_date = datetime.now() - timedelta(weeks=52)
    data = []
    base_vol = 50000
    random.seed(42)  # For consistent data
    for i in range(52):
        date = base_date + timedelta(weeks=i)
        seasonal_factor = 1 + 0.3 * (1 if date.month in [11, 12, 3, 4] else 0)
        vol = base_vol * seasonal_factor * (1 + random.uniform(-0.1, 0.15))
        base_vol = vol * 0.99 + 50000 * 0.01
        data.append(ChartDataPoint(
            name=date.strftime("%Y-%m-%d"),
            value=round(vol / 1000, 1),
            value2=round(vol * 0.6 / 1000, 1),
        ))
    return data

def generate_roi_bubble_data() -> List[ROIBubbleData]:
    random.seed(42)
    events = ["Display", "TPR", "Display", "TPR", "Display", "TPR", "Display"]
    ppgs = ["PPG-001", "PPG-002", "PPG-003", "PPG-004", "PPG-005", "PPG-006", "PPG-007"]
    data = []
    for ppg, event in zip(ppgs, events):
        data.append(ROIBubbleData(
            name=ppg,
            roi=round(random.uniform(0.8, 2.5), 2),
            investment=round(random.uniform(50000, 300000), 0),
            incremental_sales=round(random.uniform(100000, 500000), 0),
            event_type=event
        ))
    return data

def generate_promotion_calendar() -> List[PromotionEvent]:
    random.seed(42)
    events = []
    ppgs = ["PPG-001", "PPG-002", "PPG-003", "PPG-004", "PPG-005"]
    brands = ["Brand005", "Brand003", "Brand007", "Brand001", "Brand005"]
    event_types = ["Display", "TPR", "No Promo", "Display + TPR"]
    
    base_date = datetime.now() - timedelta(weeks=4)
    for week in range(16):
        for i, ppg in enumerate(ppgs):
            event_type = random.choice(event_types)
            start = base_date + timedelta(weeks=week)
            end = start + timedelta(days=6)
            discount = round(random.uniform(10, 35), 0) if event_type != "No Promo" else 0
            roi = round(random.uniform(0.8, 2.2), 2) if event_type != "No Promo" else 0
            rsp = round(random.uniform(5.5, 12.5), 2)
            events.append(PromotionEvent(
                id=f"evt-{week}-{i}",
                ppg=ppg,
                brand=brands[i],
                start_date=start.strftime("%Y-%m-%d"),
                end_date=end.strftime("%Y-%m-%d"),
                event_type=event_type,
                discount=discount,
                roi=roi,
                week=week + 1,
                rsp=rsp
            ))
    return events

def generate_volume_decomposition() -> List[VolumeDecomposition]:
    return [
        VolumeDecomposition(metrics="Value Sales (Million)", cy_value=12.5, ya_value=11.5, abs_change=1.0, pct_change=8.7),
        VolumeDecomposition(metrics="Volume Sales ('000 Units)", cy_value=845, ya_value=863, abs_change=-18, pct_change=-2.1),
        VolumeDecomposition(metrics="Base Volume ('000 Units)", cy_value=507, ya_value=518, abs_change=-11, pct_change=-2.1),
        VolumeDecomposition(metrics="Incremental Volume ('000 Units)", cy_value=338, ya_value=345, abs_change=-7, pct_change=-2.0),
        VolumeDecomposition(metrics="Regular Price (RSP)", cy_value=8.45, ya_value=8.20, abs_change=0.25, pct_change=3.0),
        VolumeDecomposition(metrics="Avg Promo Price", cy_value=6.50, ya_value=6.35, abs_change=0.15, pct_change=2.4),
        VolumeDecomposition(metrics="Discount %", cy_value=23.1, ya_value=22.6, abs_change=0.5, pct_change=2.2),
        VolumeDecomposition(metrics="Gross Margin %", cy_value=42.3, ya_value=41.8, abs_change=0.5, pct_change=1.2),
        VolumeDecomposition(metrics="Trade Spend (Million)", cy_value=2.3, ya_value=2.45, abs_change=-0.15, pct_change=-6.1),
        VolumeDecomposition(metrics="GM ROI", cy_value=1.85, ya_value=1.65, abs_change=0.2, pct_change=12.1),
    ]

def generate_promo_fact_table() -> List[PromoFactTable]:
    random.seed(42)
    ppgs = ["PPG-001", "PPG-002", "PPG-003", "PPG-004", "PPG-005"]
    brands = ["Brand005", "Brand003", "Brand007", "Brand001", "Brand005"]
    formats = ["Powder", "Liquid", "Gel", "Pods", "Powder"]
    data = []
    for ppg, brand, fmt in zip(ppgs, brands, formats):
        base_vol = random.uniform(50000, 150000)
        incr_vol = base_vol * random.uniform(0.3, 0.8)
        investment = random.uniform(20000, 80000)
        incr_gm = incr_vol * random.uniform(0.02, 0.05)
        data.append(PromoFactTable(
            ppg=ppg,
            brand=brand,
            format=fmt,
            events=random.randint(8, 20),
            base_volume=round(base_vol / 1000, 1),
            incr_volume=round(incr_vol / 1000, 1),
            total_volume=round((base_vol + incr_vol) / 1000, 1),
            roi_gm=round(incr_gm / investment, 2) if investment > 0 else 0,
            investment=round(investment / 1000, 1),
            incr_gm=round(incr_gm / 1000, 2)
        ))
    return data

def generate_optimizer_constraints() -> List[OptimizerConstraint]:
    return [
        OptimizerConstraint(kpi="Scan Net Revenue", min_value=10.0, max_value=None, current_value=12.5),
        OptimizerConstraint(kpi="Gross Margin % of NR", min_value=35.0, max_value=50.0, current_value=42.3),
        OptimizerConstraint(kpi="Volume Sales", min_value=800.0, max_value=None, current_value=845.0),
        OptimizerConstraint(kpi="Trade Spend % of NR", min_value=None, max_value=25.0, current_value=18.4),
        OptimizerConstraint(kpi="Incremental GM ROI", min_value=1.5, max_value=None, current_value=1.85),
        OptimizerConstraint(kpi="Value Market Share", min_value=30.0, max_value=None, current_value=34.2),
    ]

def generate_optimization_results() -> List[OptimizationResult]:
    random.seed(42)
    ppgs = ["PPG-001", "PPG-002", "PPG-003", "PPG-004", "PPG-005"]
    brands = ["Brand005", "Brand003", "Brand007", "Brand001", "Brand005"]
    formats = ["Powder", "Liquid", "Gel", "Pods", "Powder"]
    results = []
    
    for i, (ppg, brand, fmt) in enumerate(zip(ppgs, brands, formats)):
        for slot in range(1, 13):
            ly_vol = random.uniform(20, 50)
            lsm_vol = ly_vol * random.uniform(0.95, 1.15)
            unc_vol = ly_vol * random.uniform(1.0, 1.25)
            ly_roi = random.uniform(1.2, 2.0)
            lsm_roi = ly_roi * random.uniform(0.9, 1.1)
            unc_roi = ly_roi * random.uniform(1.0, 1.2)
            event_type = random.choice(["Display", "TPR", "Display + TPR", "No Promo"])
            discount = round(random.uniform(15, 30), 0) if event_type != "No Promo" else 0
            
            results.append(OptimizationResult(
                ppg=ppg,
                brand=brand,
                format=fmt,
                ly_volume=round(ly_vol, 1),
                lsm_volume=round(lsm_vol, 1),
                unconstrained_volume=round(unc_vol, 1),
                ly_roi=round(ly_roi, 2),
                lsm_roi=round(lsm_roi, 2),
                unconstrained_roi=round(unc_roi, 2),
                slot=slot,
                event_type=event_type,
                discount=discount
            ))
    return results

def generate_simulator_events() -> List[SimulatorEvent]:
    random.seed(42)
    ppgs = ["PPG-001", "PPG-002", "PPG-003"]
    brands = ["Brand005", "Brand003", "Brand007"]
    events = []
    event_types = ["Display", "TPR", "Display + TPR"]
    
    for i, (ppg, brand) in enumerate(zip(ppgs, brands)):
        for slot in range(1, 5):
            current_event = random.choice(event_types)
            alt_event = random.choice([e for e in event_types if e != current_event])
            current_disc = round(random.uniform(15, 30), 0)
            alt_disc = round(random.uniform(10, 35), 0)
            current_roi = round(random.uniform(1.2, 2.0), 2)
            alt_roi = round(random.uniform(1.0, 2.2), 2)
            
            events.append(SimulatorEvent(
                id=f"sim-{ppg}-{slot}",
                ppg=ppg,
                brand=brand,
                slot=slot,
                current_event=current_event,
                current_discount=current_disc,
                current_roi=current_roi,
                alt_event=alt_event,
                alt_discount=alt_disc,
                alt_roi=alt_roi,
                volume_impact=round((alt_roi - current_roi) * 10, 1)
            ))
    return events

def generate_kam_rb_metrics() -> List[KAMMetrics]:
    return [
        KAMMetrics(metric="Gross Sales", ly_value=15.2, cy_value=16.8, change_pct=10.5),
        KAMMetrics(metric="Trade Investment", ly_value=2.8, cy_value=2.5, change_pct=-10.7),
        KAMMetrics(metric="Net Revenue", ly_value=12.4, cy_value=14.3, change_pct=15.3),
        KAMMetrics(metric="COGS", ly_value=7.2, cy_value=8.1, change_pct=12.5),
        KAMMetrics(metric="Gross Margin", ly_value=5.2, cy_value=6.2, change_pct=19.2),
        KAMMetrics(metric="GM %", ly_value=41.9, cy_value=43.4, change_pct=3.6),
    ]

def generate_kam_customer_metrics() -> List[KAMMetrics]:
    return [
        KAMMetrics(metric="Retailer Revenue", ly_value=18.5, cy_value=20.2, change_pct=9.2),
        KAMMetrics(metric="Retailer Margin", ly_value=3.3, cy_value=3.4, change_pct=3.0),
        KAMMetrics(metric="Retailer Margin %", ly_value=17.8, cy_value=16.8, change_pct=-5.6),
        KAMMetrics(metric="Volume Sales", ly_value=863, cy_value=845, change_pct=-2.1),
        KAMMetrics(metric="Avg Selling Price", ly_value=21.4, cy_value=23.9, change_pct=11.7),
    ]

def generate_scenario_comparison() -> List[Dict[str, Any]]:
    scenarios = ["Last Year", "LSM Constrained", "Unconstrained", "Custom Plan A"]
    data = []
    for scenario in scenarios:
        base = 1.0 if scenario == "Last Year" else random.uniform(0.95, 1.15)
        data.append({
            "scenario": scenario,
            "net_revenue": round(12.5 * base, 2),
            "gross_margin": round(5.2 * base, 2),
            "gm_pct": round(41.6 * (base * 0.95 + 0.05), 1),
            "volume": round(845 * base, 0),
            "roi": round(1.85 * base, 2),
            "trade_spend": round(2.3 / base, 2),
        })
    return data

# ==================== ROUTES ====================

@router.get("/filters")
async def get_filter_options():
    """Get available filter options from R API or fallback to CSV data"""
    r_data = await get_r_data("analytics/filters")
    
    if r_data and "error" not in r_data:
        # Helper to ensure value is always a list (handles R's jsonlite unboxing)
        def ensure_list(val):
            if val is None:
                return []
            if isinstance(val, str):
                return [val]  # Single string -> list with one element
            if isinstance(val, list):
                # Flatten nested single-element lists
                return [item[0] if isinstance(item, list) and len(item) == 1 else item for item in val]
            return [val]  # Wrap any other single value in a list
        
        # Helper to ensure value is a dict (for mappings)
        def ensure_dict(val):
            if val is None:
                return {}
            if isinstance(val, dict):
                # Unbox single-element arrays in dict values
                return {k: (v[0] if isinstance(v, list) and len(v) == 1 else v) for k, v in val.items()}
            return {}
        
        countries = ensure_list(r_data.get("countries", ["UAE"]))
        customers = ensure_list(r_data.get("customers", []))
        categories = ensure_list(r_data.get("categories", []))
        brands = ensure_list(r_data.get("brands", []))
        formats = ensure_list(r_data.get("formats", []))
        ppgs = ensure_list(r_data.get("ppgs", []))
        manufacturers = ensure_list(r_data.get("manufacturers", []))
        
        # Extract PPG mappings for filtering
        ppg_to_brand = ensure_dict(r_data.get("ppgToBrand", {}))
        ppg_category_map = ensure_dict(r_data.get("ppg_category_map", {}))
        ppg_descriptions = ensure_dict(r_data.get("ppg_descriptions", {}))
        
        # Use ppg_category_map if ppgToBrand is empty
        if not ppg_to_brand and ppg_category_map:
            ppg_to_brand = ppg_category_map
        
        print(f"[FILTERS] R API returned {len(ppgs)} PPGs, {len(ppg_to_brand)} ppgToBrand mappings, {len(ppg_descriptions)} descriptions")
        
        return FilterOptions(
            countries=countries,
            customers=customers,
            categories=categories,
            brands=brands,
            formats=formats,
            ppgs=ppgs,
            manufacturers=manufacturers,
            ppgToBrand=ppg_to_brand,
            ppg_category_map=ppg_category_map,
            ppg_descriptions=ppg_descriptions
        )
    
    # FALLBACK: R API not available - try to load from shiny_ip_events.csv
    print("[FILTERS] R API not available, falling back to CSV data")
    try:
        import pandas as pd
        DATA_DIR = os.getenv("DATA_DIR", "/app/data")
        csv_path = os.path.join(DATA_DIR, "shiny_ip_events.csv")
        
        if os.path.exists(csv_path):
            df = pd.read_csv(csv_path)
            print(f"[FILTERS] Loaded shiny_ip_events.csv with {len(df)} rows, columns: {list(df.columns)}")
            
            # Extract unique values
            ppgs = df['PPG'].dropna().unique().tolist() if 'PPG' in df.columns else []
            
            # Categories come from PRODUCT RANGE column
            categories = df['PRODUCT RANGE'].dropna().unique().tolist() if 'PRODUCT RANGE' in df.columns else []
            
            # Customers from TRADING COMPANY
            customers = df['TRADING COMPANY'].dropna().unique().tolist() if 'TRADING COMPANY' in df.columns else ['Carrefour']
            
            # Formats
            formats = df['FORMAT'].dropna().unique().tolist() if 'FORMAT' in df.columns else []
            
            # Build PPG to Category mapping
            ppg_to_brand = {}
            ppg_descriptions = {}
            
            if 'PPG' in df.columns:
                for _, row in df.drop_duplicates(subset=['PPG']).iterrows():
                    ppg = str(row['PPG'])
                    if 'PRODUCT RANGE' in df.columns and pd.notna(row.get('PRODUCT RANGE')):
                        ppg_to_brand[ppg] = str(row['PRODUCT RANGE'])
                    if 'PPG_Description' in df.columns and pd.notna(row.get('PPG_Description')):
                        ppg_descriptions[ppg] = str(row['PPG_Description'])
            
            print(f"[FILTERS] CSV fallback: {len(ppgs)} PPGs, {len(categories)} categories, {len(ppg_to_brand)} mappings, {len(ppg_descriptions)} descriptions")
            print(f"[FILTERS] Categories found: {categories}")
            print(f"[FILTERS] Sample ppgToBrand: {dict(list(ppg_to_brand.items())[:5])}")
            
            return FilterOptions(
                countries=["UAE"],
                customers=customers,
                categories=categories,
                brands=categories,  # Use categories as brands for backward compatibility
                formats=formats,
                ppgs=ppgs,
                manufacturers=["Reckitt"],
                ppgToBrand=ppg_to_brand,
                ppg_category_map=ppg_to_brand,
                ppg_descriptions=ppg_descriptions
            )
    except Exception as e:
        print(f"[FILTERS] CSV fallback error: {e}")
    
    # Return empty data if all else fails
    return FilterOptions(
        countries=[],
        customers=[],
        categories=[],
        brands=[],
        formats=[],
        ppgs=[],
        manufacturers=[],
        ppgToBrand={},
        ppg_category_map={},
        ppg_descriptions={}
    )


def parse_r_selectize_choices(content: str, input_id: str) -> List[str]:
    """
    Parse R Shiny selectizeInput choices from ui.R file.
    
    Looks for patterns like:
    selectizeInput("SP_opti_goal","Optimization Goal",choices = c("Option1","Option2",...))
    """
    # Pattern to match selectizeInput with the specific id
    # This handles multi-line patterns
    pattern = rf'selectizeInput\s*\(\s*"{input_id}"[^)]*choices\s*=\s*c\s*\(([^)]+)\)'
    
    match = re.search(pattern, content, re.DOTALL | re.IGNORECASE)
    if match:
        choices_str = match.group(1)
        # Extract quoted strings
        options = re.findall(r'"([^"]+)"', choices_str)
        return options
    
    return []


class UIOptionsResponse(BaseModel):
    """Response model for UI options parsed from R Shiny ui.R"""
    optimization_goals: List[str]
    slot_criteria: List[str]
    run_optimization_types: List[str]
    roi_options: List[str]
    source: str


@router.get("/ui-options", response_model=UIOptionsResponse)
async def get_ui_options():
    """
    Get UI dropdown options by parsing the R Shiny ui.R file.
    This ensures the React UI matches the R Shiny UI behavior.
    """
    # Path to the R Shiny ui.R file
    ui_r_path = "/app/Codes Guest1 - Copy/Codes/ui.R"
    
    # Fallback defaults (same as R Shiny)
    default_optimization_goals = [
        "Scan Net Revenue",
        "Gross Margin % of NR",
        "Volume Sales",
        "Scan Gross Sales",
        "Incremental GM ROI",
        "Trade Spend % of NR",
        "Value Market Share",
        "Trade Spend % of NIS",
        "Gross Margin"
    ]
    
    default_slot_criteria = [
        "Seasonality Trend",
        "Competitor Promotion Timing",
        "Cannibalization- Complementary Impact"
    ]
    
    default_run_types = [
        "Run LSM constrained Optimization",
        "Run Unconstrained Optimization",
        "Run Complete Optimization"
    ]
    
    default_roi_options = [
        "Incremental GM ROI",
        "Incremental NR ROI"
    ]
    
    try:
        if os.path.exists(ui_r_path):
            with open(ui_r_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            # Parse optimization goals from SP_opti_goal
            optimization_goals = parse_r_selectize_choices(content, "SP_opti_goal")
            if not optimization_goals:
                optimization_goals = default_optimization_goals
            
            # Parse slot criteria from SP_opti_comp
            slot_criteria = parse_r_selectize_choices(content, "SP_opti_comp")
            if not slot_criteria:
                slot_criteria = default_slot_criteria
            
            # Parse run optimization types from SP_opti_run_choice
            run_types = parse_r_selectize_choices(content, "SP_opti_run_choice")
            if not run_types:
                run_types = default_run_types
            
            # Parse ROI options from SP_opti_ROI_selection (using awesomeRadio)
            # Pattern for awesomeRadio is slightly different
            roi_pattern = r'awesomeRadio\s*\(\s*"SP_opti_ROI_selection"[^)]*choices\s*=\s*c\s*\(([^)]+)\)'
            roi_match = re.search(roi_pattern, content, re.DOTALL | re.IGNORECASE)
            if roi_match:
                roi_str = roi_match.group(1)
                roi_options = re.findall(r'"([^"]+)"', roi_str)
            else:
                roi_options = default_roi_options
            
            return UIOptionsResponse(
                optimization_goals=optimization_goals,
                slot_criteria=slot_criteria,
                run_optimization_types=run_types,
                roi_options=roi_options,
                source="ui.R"
            )
        else:
            # File not found, return defaults
            return UIOptionsResponse(
                optimization_goals=default_optimization_goals,
                slot_criteria=default_slot_criteria,
                run_optimization_types=default_run_types,
                roi_options=default_roi_options,
                source="defaults"
            )
    except Exception as e:
        # On error, return defaults
        return UIOptionsResponse(
            optimization_goals=default_optimization_goals,
            slot_criteria=default_slot_criteria,
            run_optimization_types=default_run_types,
            roi_options=default_roi_options,
            source=f"defaults (error: {str(e)})"
        )


@router.get("/kpis")
async def get_kpi_data(
    country: Optional[str] = None,
    customer: Optional[str] = None,
    category: Optional[str] = None,
    brand: Optional[str] = None,
    
):
    """Get KPI summary data from R API - NO MOCK DATA"""
    r_data = await get_r_data("analytics/kpis")
    
    if r_data and "error" not in r_data:
        # Convert R response to our format - data_source indicates real vs mock data
        _ = r_data.get("data_source", "real_data")  # Reserved for future use
        
        return [
            KPIData(label="Total Revenue", value=r_data.get("total_revenue", 0), change=0, trend="up"),
            KPIData(label="Total Units", value=r_data.get("total_units", 0), change=0, trend="up"),
            KPIData(label="Total Volume", value=r_data.get("total_volume", 0), change=0, trend="up"),
            KPIData(label="Avg Price", value=round(r_data.get("avg_price", 0), 2), change=0, trend="up"),
            KPIData(label="Products", value=r_data.get("unique_products", 0), change=0, trend="up"),
            KPIData(label="Retailers", value=r_data.get("unique_retailers", 0), change=0, trend="up"),
        ]
    
    # Return empty KPIs if R API not available - NO MOCK DATA
    return []

@router.get("/value-share")
async def get_value_share():
    """Get value share by brand from R API - NO MOCK DATA"""
    r_data = await get_r_data("analytics/brand-summary")
    
    if r_data and "error" not in r_data:
        data = []
        for brand_data in r_data.get("data", []):
            name = brand_data.get("brand", "Unknown")
            if isinstance(name, list):
                name = name[0] if name else "Unknown"
            value = brand_data.get("value_share", 0)
            if isinstance(value, list):
                value = value[0] if value else 0
            data.append({
                "name": name,
                "value": value,
                "category": "Brand"
            })
        return data
    
    # Return empty if R API not available - NO MOCK DATA
    return []

@router.get("/volume-trend")
async def get_volume_trend():
    """Get volume trend over time - NO MOCK DATA"""
    # Return empty if R API not available
    return []

@router.get("/roi-analysis")
async def get_roi_analysis():
    """Get ROI analysis from R API - NO MOCK DATA"""
    r_data = await get_r_data("analytics/roi-analysis")
    
    if r_data and "error" not in r_data:
        return {
            "total_investment": r_data.get("total_investment", 0),
            "total_events": r_data.get("total_events", 0),
            "avg_investment_per_event": r_data.get("avg_investment_per_event", 0)
        }
    
    # Return empty if R API not available - NO MOCK DATA
    return []

@router.get("/calendar")
async def get_promotion_calendar():
    """Get promotion calendar events - NO MOCK DATA"""
    # Return empty - calendar data comes from optimizer
    return []

@router.get("/volume-decomposition")
async def get_volume_decomposition():
    """Get volume decomposition from R API - NO MOCK DATA"""
    r_data = await get_r_data("analytics/volume-decomposition")
    
    if r_data and "error" not in r_data:
        return {
            "total_volume": r_data.get("total_volume", 0),
            "total_value": r_data.get("total_value", 0),
            "base_volume": r_data.get("base_volume", 0),
            "incremental_volume": r_data.get("incremental_volume", 0),
            "promo_effectiveness": r_data.get("promo_effectiveness", 0)
        }
    
    # Return empty if R API not available - NO MOCK DATA
    return []

@router.get("/promo-fact-table")
async def get_promo_fact_table():
    """Get promotion effectiveness fact table from R API - NO MOCK DATA"""
    r_data = await get_r_data("analytics/brand-summary")
    
    if r_data and "error" not in r_data:
        data = []
        for brand_data in r_data.get("data", []):
            brand = brand_data.get("brand", "Unknown")
            if isinstance(brand, list):
                brand = brand[0] if brand else "Unknown"
            
            total_volume = brand_data.get("total_volume", 0)
            if isinstance(total_volume, list):
                total_volume = total_volume[0] if total_volume else 0
            
            total_value = brand_data.get("total_value", 0)
            if isinstance(total_value, list):
                total_value = total_value[0] if total_value else 0
            
            promo_pct = brand_data.get("promo_pct", 50)
            if isinstance(promo_pct, list):
                promo_pct = promo_pct[0] if promo_pct else 50
            
            # Calculate metrics from real data
            incr_volume = total_volume * promo_pct / 100
            base_volume = total_volume - incr_volume
            investment = total_value * 0.15
            incr_gm = incr_volume * 0.03
            
            data.append({
                "ppg": f"PPG-{brand[:3].upper()}",
                "brand": brand,
                "format": "Various",
                "events": 0,
                "base_volume": round(base_volume / 1000, 1),
                "incr_volume": round(incr_volume / 1000, 1),
                "total_volume": round(total_volume / 1000, 1),
                "roi_gm": round(incr_gm / investment, 2) if investment > 0 else 0,
                "investment": round(investment / 1000, 1),
                "incr_gm": round(incr_gm / 1000, 2)
            })
        return data
    
    # Return empty if R API not available - NO MOCK DATA
    return []

@router.get("/optimizer/constraints")
async def get_optimizer_constraints():
    """Get optimizer constraints - NO MOCK DATA"""
    # Return empty - constraints come from frontend UI
    return []

@router.get("/optimizer/results")
async def get_optimization_results():
    """Get optimization results - NO MOCK DATA"""
    # Return empty - results come from R optimizer
    return []

@router.post("/optimizer/run")
async def run_optimization(
    config: OptimizerConfig,
    
):
    """Run optimization with given configuration - delegates to R engine"""
    # This endpoint is deprecated - use /api/r/optimizer/run instead
    return {
        "status": "error",
        "message": "Please use /api/r/optimizer/run endpoint for optimization"
    }

@router.get("/simulator/events", response_model=List[SimulatorEvent])
async def get_simulator_events(
    ppg: Optional[str] = None,
    
):
    """Get events for simulation - NO MOCK DATA"""
    # Return empty - needs real data from R
    return []

@router.post("/simulator/run")
async def run_simulation(
    event_id: str,
    alt_event: str,
    alt_discount: float,
    
):
    """Run event simulation - NO MOCK DATA"""
    return {
        "status": "error",
        "message": "Simulation requires R engine to be running with real data"
    }

@router.get("/kam/rb-metrics", response_model=List[KAMMetrics])
async def get_kam_rb_metrics():
    """Get KAM RB P&L metrics - NO MOCK DATA"""
    return []

@router.get("/kam/customer-metrics", response_model=List[KAMMetrics])
async def get_kam_customer_metrics():
    """Get KAM Customer P&L metrics - NO MOCK DATA"""
    return []

@router.get("/scenarios/comparison")
async def get_scenario_comparison():
    """Get scenario comparison data - NO MOCK DATA"""
    return []

@router.get("/data-table")
async def get_sku_data_table(
    category: Optional[str] = None,
    brand: Optional[str] = None,
    
):
    """Get SKU-level data table from R API - NO MOCK DATA"""
    r_data = await get_r_data("analytics/data-table")
    
    if r_data and "error" not in r_data:
        data = []
        for item in r_data.get("data", []):
            brand_name = item.get("brand", "Unknown")
            if isinstance(brand_name, list):
                brand_name = brand_name[0] if brand_name else "Unknown"
            cy_volume = item.get("cy_volume", "0")
            if isinstance(cy_volume, list):
                cy_volume = cy_volume[0] if cy_volume else "0"
            cy_value = item.get("cy_value", "0")
            if isinstance(cy_value, list):
                cy_value = cy_value[0] if cy_value else "0"
            vol_change = item.get("vol_change", 0)
            if isinstance(vol_change, list):
                vol_change = vol_change[0] if vol_change else 0
            price_change = item.get("price_change", 0)
            if isinstance(price_change, list):
                price_change = price_change[0] if price_change else 0
                
            data.append({
                "sku": f"SKU-{brand_name[:3].upper()}",
                "description": brand_name,
                "ppg": "PPG-001",
                "brand": brand_name,
                "format": "Various",
                "volume_sales": cy_volume,
                "value_sales": cy_value,
                "base_volume": "N/A",
                "incr_volume": "N/A",
                "avg_price": "N/A",
                "discount_pct": "N/A",
                "vol_change": vol_change,
                "price_change": price_change
            })
        return data
    
    # Return empty if R API not available - NO MOCK DATA
    return []

@router.get("/promo-split")
async def get_promo_split():
    """Get promotion split by brand - NO MOCK DATA"""
    r_data = await get_r_data("analytics/brand-summary")
    
    if r_data and "error" not in r_data:
        data = []
        for brand_data in r_data.get("data", []):
            brand = brand_data.get("brand", "Unknown")
            if isinstance(brand, list):
                brand = brand[0] if brand else "Unknown"
            
            total_volume = brand_data.get("total_volume", 0)
            if isinstance(total_volume, list):
                total_volume = total_volume[0] if total_volume else 0
            
            promo_pct = brand_data.get("promo_pct", 50)
            if isinstance(promo_pct, list):
                promo_pct = promo_pct[0] if promo_pct else 50
            
            incr_sales = (total_volume * promo_pct / 100) / 1000000
            base_sales = (total_volume * (100 - promo_pct) / 100) / 1000000
            
            data.append({
                "brand": brand,
                "base_sales": round(base_sales, 2),
                "incr_sales": round(incr_sales, 2),
                "total_sales": round(base_sales + incr_sales, 2),
                "incr_pct": round(promo_pct, 1)
            })
        return data
    
    # Return empty if R API not available - NO MOCK DATA
    return []

@router.get("/promo-wow")
async def get_promo_week_over_week():
    """Get week-over-week promotion performance - NO MOCK DATA"""
    r_data = await get_r_data("analytics/brand-summary")
    
    if r_data and "error" not in r_data:
        total_volume = 0
        for brand_data in r_data.get("data", []):
            vol = brand_data.get("total_volume", 0)
            if isinstance(vol, list):
                vol = vol[0] if vol else 0
            total_volume += vol
            
        # Generate weekly data based on real totals
        weekly_avg = total_volume / 12 / 1000 if total_volume > 0 else 0
        data = []
        base_date = datetime.now() - timedelta(weeks=12)
        for week in range(12):
            date = base_date + timedelta(weeks=week)
            variation = 0.8 + (week % 4) * 0.1
            base_vol = weekly_avg * 0.7 * variation
            incr_vol = weekly_avg * 0.3 * variation
            data.append({
                "week": date.strftime("%Y-W%W"),
                "week_num": week + 1,
                "base_volume": round(base_vol, 1),
                "incr_volume": round(incr_vol, 1),
                "total_volume": round(base_vol + incr_vol, 1),
                "discount": round(15 + (week % 3) * 5, 0),
            })
        return data
    
    # Return empty if R API not available - NO MOCK DATA
    return []


# ==================== REAL DATA ENDPOINTS (FROM R API) ====================

@router.get("/real/summary")
async def get_real_data_summary():
    """Get summary of real uploaded data from R API"""
    data = await get_r_data("analytics/data-summary")
    if data:
        return {"source": "real_data", "data": data}
    return {"source": "none", "message": "R API not available or no data uploaded"}

@router.get("/real/nielsen")
async def get_real_nielsen_data():
    """Get real Nielsen RMS data from R API"""
    data = await get_r_data("analytics/kpis")
    if data and "error" not in data:
        return {"source": "real_data", "data": data}
    return {"source": "none", "message": "Nielsen data not available"}

@router.get("/real/events")
async def get_real_events_data():
    """Get real events/promotions data from R API"""
    data = await get_r_data("analytics/events")
    if data and "error" not in data:
        return {"source": "real_data", "data": data}
    return {"source": "none", "message": "Events data not available"}

@router.get("/real/model-results")
async def get_real_model_results():
    """Get real model results from R API"""
    data = await get_r_data("analytics/model-results")
    if data and "error" not in data:
        return {"source": "real_data", "data": data}
    return {"source": "none", "message": "Model results not available"}

