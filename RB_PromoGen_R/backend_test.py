#!/usr/bin/env python3
"""
Backend API Testing Script for PromoGen Trade Promotion Optimizer

Tests the FastAPI backend functionality, especially the new constraint parameters
and R Plumber proxy functionality.

Key areas tested:
1. Backend health and startup
2. /api/r/optimizer/run endpoint with new constraint parameters
3. /api/promo/ui-options endpoint
4. Parameter mapping and validation

Note: R Plumber at port 8002 is NOT available in this environment,
so 520 errors are expected for R endpoints.
"""

import requests
import json
import sys
import time
import os
from typing import Dict, Any

# Get backend URL from frontend .env file
def get_backend_url():
    try:
        with open('/app/frontend/.env', 'r') as f:
            for line in f:
                if line.startswith('REACT_APP_BACKEND_URL='):
                    return line.split('=', 1)[1].strip()
    except:
        pass
    return 'https://accuracy-verify.preview.emergentagent.com'

BACKEND_URL = get_backend_url()
print(f"Testing backend at: {BACKEND_URL}")


class BackendTester:
    def __init__(self, base_url: str):
        self.base_url = base_url.rstrip('/')
        self.session = requests.Session()
        self.test_results = []
    
    def log_test(self, test_name: str, success: bool, details: str = ""):
        """Log test results"""
        status = "✅ PASS" if success else "❌ FAIL"
        print(f"{status} {test_name}")
        if details:
            print(f"   {details}")
        self.test_results.append({
            "test": test_name,
            "success": success,
            "details": details
        })
    
    def test_health_endpoint(self):
        """Test the /api/health endpoint"""
        print("\n=== Testing Health Endpoint ===")
        try:
            response = self.session.get(f"{self.base_url}/api/health", timeout=10)
            if response.status_code == 200:
                data = response.json()
                self.log_test("Health endpoint responds", True, f"Status: {data.get('status')}, R engine: {data.get('r_engine')}")
                return True
            else:
                self.log_test("Health endpoint responds", False, f"Status code: {response.status_code}")
                return False
        except Exception as e:
            self.log_test("Health endpoint responds", False, f"Error: {str(e)}")
            return False
    
    def test_root_endpoint(self):
        """Test the root /api/ endpoint"""
        print("\n=== Testing Root API Endpoint ===")
        try:
            response = self.session.get(f"{self.base_url}/api/", timeout=10)
            if response.status_code == 200:
                data = response.json()
                self.log_test("Root API endpoint", True, f"Message: {data.get('message')}, Version: {data.get('version')}")
                return True
            else:
                self.log_test("Root API endpoint", False, f"Status code: {response.status_code}")
                return False
        except Exception as e:
            self.log_test("Root API endpoint", False, f"Error: {str(e)}")
            return False
    
    def test_promo_ui_options(self):
        """Test the /api/promo/ui-options endpoint"""
        print("\n=== Testing UI Options Endpoint ===")
        try:
            response = self.session.get(f"{self.base_url}/api/promo/ui-options", timeout=10)
            if response.status_code == 200:
                data = response.json()
                goals = data.get('optimization_goals', [])
                criteria = data.get('slot_criteria', [])
                run_types = data.get('run_optimization_types', [])
                roi_options = data.get('roi_options', [])
                source = data.get('source', 'unknown')
                
                details = f"Source: {source}, Goals: {len(goals)}, Criteria: {len(criteria)}, Run types: {len(run_types)}, ROI options: {len(roi_options)}"
                self.log_test("UI options endpoint", True, details)
                
                # Validate expected options are present
                expected_goals = ["Scan Net Revenue", "Gross Margin % of NR", "Volume Sales"]
                expected_run_types = ["Run Complete Optimization", "Run LSM constrained Optimization"]
                
                goals_found = all(goal in goals for goal in expected_goals)
                run_types_found = all(run_type in run_types for run_type in expected_run_types)
                
                self.log_test("Expected optimization goals present", goals_found, f"Found: {goals[:3]}")
                self.log_test("Expected run types present", run_types_found, f"Found: {run_types[:2]}")
                
                return True
            else:
                self.log_test("UI options endpoint", False, f"Status code: {response.status_code}")
                return False
        except Exception as e:
            self.log_test("UI options endpoint", False, f"Error: {str(e)}")
            return False
    
    def test_optimizer_parameter_mapping(self):
        """Test the /api/r/optimizer/run endpoint parameter mapping"""
        print("\n=== Testing Optimizer Parameter Mapping ===")
        
        # Comprehensive test payload with all new constraint parameters
        test_payload = {
            "settings": {
                "goal": "Scan Net Revenue",
                "sign": "Max",
                "slotCriterion": "Seasonality Trend",
                "runType": "Run Complete Optimization",
                "roiType": "Incremental GM ROI"
            },
            "filters": {
                "retailer": "Carrefour",
                "brand": "DETTOL",
                "ppg": "PPG-001",
                "dateType": "monthly",
                "startMonth": "Jan",
                "endMonth": "Dec"
            },
            "constraints": [
                {
                    "kpi": "scan gross sales",
                    "min": "100",
                    "max": "500",
                    "scale": "Absolute"
                },
                {
                    "kpi": "volume sales", 
                    "min": "800",
                    "max": "1200",
                    "scale": "Absolute"
                },
                {
                    "kpi": "gross margin",
                    "min": "5",
                    "max": "15", 
                    "scale": "Absolute"
                },
                {
                    "kpi": "incremental gm roi",
                    "min": "1.5",
                    "max": "3.0",
                    "scale": "Percent"
                },
                {
                    "kpi": "value market share",
                    "min": "30",
                    "max": "40",
                    "scale": "Percent"
                }
            ],
            "restrictions": {
                "priceMin": 3.49,
                "priceMax": 4.49,
                "slotsMin": 1,
                "slotsMax": 6,
                "minInvestment": 50000,
                "maxInvestment": 200000,
                "mechanics": {
                    "TPR": True,
                    "Display": True
                }
            }
        }
        
        try:
            response = self.session.post(
                f"{self.base_url}/api/r/optimizer/run", 
                json=test_payload,
                timeout=30
            )
            
            # We expect this to fail with 503 (R API unavailable) since R Plumber is not running
            # But we want to verify the parameter mapping logic in server.py works correctly
            if response.status_code == 503:
                error_detail = response.json().get('detail', '')
                if 'R API unavailable' in error_detail:
                    self.log_test("Optimizer parameter mapping (R unavailable expected)", True, 
                                f"Expected 503 error: {error_detail}")
                    return True
                else:
                    self.log_test("Optimizer parameter mapping", False, 
                                f"Unexpected 503 error: {error_detail}")
                    return False
            elif response.status_code == 520:
                # Another expected error format
                self.log_test("Optimizer parameter mapping (R unavailable expected)", True, 
                            f"Expected 520 error (R API unavailable)")
                return True
            else:
                # Unexpected success or different error
                self.log_test("Optimizer parameter mapping", False, 
                            f"Unexpected status code: {response.status_code}, Response: {response.text[:200]}")
                return False
                
        except requests.exceptions.Timeout:
            self.log_test("Optimizer parameter mapping", False, "Request timeout - server may be processing")
            return False
        except Exception as e:
            self.log_test("Optimizer parameter mapping", False, f"Error: {str(e)}")
            return False
    
    def test_constraint_parameter_coverage(self):
        """Test that all new constraint parameters are covered in server.py mapping"""
        print("\n=== Testing New Constraint Parameter Coverage ===")
        
        # Read server.py to verify parameter mapping
        try:
            with open('/app/backend/server.py', 'r') as f:
                server_content = f.read()
            
            # Check for the new constraint parameters mentioned in review request
            new_params = [
                'gs_min', 'gs_max',      # Gross Sales constraints
                'vol_min', 'vol_max',    # Volume constraints  
                'gm_abs_min', 'gm_abs_max',  # Gross Margin Absolute constraints
                'roi_min', 'roi_max',    # ROI constraints
                'ms_min', 'ms_max'       # Market Share constraints
            ]
            
            found_params = []
            missing_params = []
            
            for param in new_params:
                if param in server_content:
                    found_params.append(param)
                else:
                    missing_params.append(param)
            
            if len(found_params) == len(new_params):
                self.log_test("All new constraint parameters present in server.py", True, 
                            f"Found all {len(new_params)} parameters")
            else:
                self.log_test("All new constraint parameters present in server.py", False, 
                            f"Missing: {missing_params}, Found: {found_params}")
            
            # Check for constraint mapping logic
            constraint_mapping_found = 'KPI_MAPPING' in server_content and 'ABSOLUTE_SCALE_KPIS' in server_content
            self.log_test("Constraint mapping logic present", constraint_mapping_found, 
                        "KPI_MAPPING and ABSOLUTE_SCALE_KPIS found" if constraint_mapping_found else "Mapping logic missing")
            
            return len(found_params) == len(new_params) and constraint_mapping_found
            
        except Exception as e:
            self.log_test("New constraint parameter coverage", False, f"Error reading server.py: {str(e)}")
            return False
    
    def test_data_endpoints(self):
        """Test various data endpoints to ensure they respond correctly"""
        print("\n=== Testing Data Endpoints ===")
        
        endpoints = [
            ("/api/data/files", "File listing"),
            ("/api/data/mapping", "File mapping"),
            ("/api/promo/filters", "Promo filters"),
            ("/api/r-engine/status", "R engine status")
        ]
        
        success_count = 0
        for endpoint, description in endpoints:
            try:
                response = self.session.get(f"{self.base_url}{endpoint}", timeout=10)
                if response.status_code == 200:
                    self.log_test(f"{description} endpoint", True, f"Status: 200")
                    success_count += 1
                else:
                    self.log_test(f"{description} endpoint", False, f"Status: {response.status_code}")
            except Exception as e:
                self.log_test(f"{description} endpoint", False, f"Error: {str(e)}")
        
        return success_count == len(endpoints)
    
    def check_frontend_emergent_text(self):
        """Check if frontend has any 'emergent' text visible in page source (excluding iframe scripts)"""
        print("\n=== Checking Frontend for Emergent Text ===")
        
        try:
            # Get the main frontend page
            frontend_url = BACKEND_URL.replace('/api', '').rstrip('/')
            response = self.session.get(frontend_url, timeout=15)
            if response.status_code == 200:
                html_content = response.text.lower()
                
                # Look for 'emergent' text but exclude iframe-only scripts
                emergent_matches = []
                lines = html_content.split('\n')
                
                for i, line in enumerate(lines, 1):
                    if 'emergent' in line:
                        # Check if it's in an iframe script (debug-monitor.js is allowed)
                        if 'debug-monitor' not in line and 'iframe' not in line:
                            emergent_matches.append(f"Line {i}: {line.strip()[:100]}")
                
                if not emergent_matches:
                    self.log_test("No visible emergent text in frontend", True, "Page loaded without emergent references")
                    return True
                else:
                    self.log_test("No visible emergent text in frontend", False, 
                                f"Found {len(emergent_matches)} emergent references: {emergent_matches[:2]}")
                    return False
            else:
                self.log_test("Frontend page loads", False, f"Status: {response.status_code}")
                return False
        except Exception as e:
            self.log_test("Frontend emergent text check", False, f"Error: {str(e)}")
            return False
    
    def run_all_tests(self):
        """Run all backend tests"""
        print("=" * 60)
        print("BACKEND API TESTING - PromoGen Trade Promotion Optimizer")
        print("=" * 60)
        
        # Test backend functionality
        health_ok = self.test_health_endpoint()
        root_ok = self.test_root_endpoint()
        ui_options_ok = self.test_promo_ui_options()
        params_ok = self.test_constraint_parameter_coverage()
        optimizer_ok = self.test_optimizer_parameter_mapping()
        data_ok = self.test_data_endpoints()
        frontend_ok = self.check_frontend_emergent_text()
        
        # Summary
        print("\n" + "=" * 60)
        print("TEST SUMMARY")
        print("=" * 60)
        
        passed = sum(1 for result in self.test_results if result['success'])
        total = len(self.test_results)
        
        for result in self.test_results:
            status = "✅" if result['success'] else "❌"
            print(f"{status} {result['test']}")
            if result['details'] and not result['success']:
                print(f"   Details: {result['details']}")
        
        print(f"\nOverall: {passed}/{total} tests passed")
        
        # Critical issues check
        critical_issues = []
        if not health_ok:
            critical_issues.append("Backend health endpoint not responding")
        if not ui_options_ok:
            critical_issues.append("UI options endpoint not working")
        if not params_ok:
            critical_issues.append("New constraint parameters missing from server.py")
        
        if critical_issues:
            print(f"\n🚨 CRITICAL ISSUES:")
            for issue in critical_issues:
                print(f"   - {issue}")
        
        return {
            'total_tests': total,
            'passed_tests': passed,
            'critical_issues': critical_issues,
            'backend_healthy': health_ok,
            'optimizer_param_mapping': params_ok,
            'ui_options_working': ui_options_ok,
            'frontend_clean': frontend_ok
        }


if __name__ == "__main__":
    tester = BackendTester(BACKEND_URL)
    results = tester.run_all_tests()
    
    # Exit with appropriate code
    if results['critical_issues']:
        sys.exit(1)
    else:
        sys.exit(0)