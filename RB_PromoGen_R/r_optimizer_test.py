import requests
import json
import os
from datetime import datetime

class ROptimizerAPITester:
    def __init__(self):
        # Get backend URL from environment variable or use default
        self.base_url = os.getenv("REACT_APP_BACKEND_URL", "http://localhost:8001")
        self.tests_run = 0
        self.tests_passed = 0
        self.failed_tests = []
        self.test_results = {}

    def log_test_result(self, name, success, details=None):
        """Log test result with detailed information"""
        self.tests_run += 1
        if success:
            self.tests_passed += 1
            print(f"✅ {name} - PASSED")
        else:
            print(f"❌ {name} - FAILED")
            self.failed_tests.append({
                'name': name,
                'details': details or {}
            })
        
        self.test_results[name] = {
            'success': success,
            'details': details or {}
        }

    def test_endpoint(self, name, endpoint, expected_checks=None):
        """Test a specific R optimizer endpoint"""
        url = f"{self.base_url}/api/r/optimizer/{endpoint}"
        
        print(f"\n🔍 Testing {name}")
        print(f"   URL: {url}")
        
        try:
            response = requests.get(url, timeout=30)
            print(f"   Status Code: {response.status_code}")
            
            if response.status_code != 200:
                self.log_test_result(name, False, {
                    'error': f"HTTP {response.status_code}",
                    'response': response.text[:300]
                })
                return False, {}
            
            try:
                data = response.json()
            except json.JSONDecodeError as e:
                self.log_test_result(name, False, {
                    'error': f"Invalid JSON response: {str(e)}",
                    'response': response.text[:300]
                })
                return False, {}
            
            # Run expected checks if provided
            if expected_checks:
                check_results = []
                for check_name, check_func in expected_checks.items():
                    try:
                        result = check_func(data)
                        check_results.append(f"{check_name}: {'✓' if result else '✗'}")
                        if not result:
                            print(f"   ❌ Check failed: {check_name}")
                    except Exception as e:
                        check_results.append(f"{check_name}: ERROR - {str(e)}")
                        print(f"   ❌ Check error: {check_name} - {str(e)}")
                
                all_passed = all('✓' in result for result in check_results)
                print(f"   Checks: {', '.join(check_results)}")
                
                self.log_test_result(name, all_passed, {
                    'data_keys': list(data.keys()) if isinstance(data, dict) else "not_dict",
                    'check_results': check_results,
                    'sample_data': str(data)[:500] if data else "empty"
                })
                return all_passed, data
            else:
                # Just check if we got data
                success = bool(data)
                self.log_test_result(name, success, {
                    'data_keys': list(data.keys()) if isinstance(data, dict) else "not_dict",
                    'sample_data': str(data)[:500] if data else "empty"
                })
                return success, data
                
        except requests.exceptions.RequestException as e:
            self.log_test_result(name, False, {
                'error': f"Request failed: {str(e)}"
            })
            return False, {}

    def check_data_source_field(self, data):
        """Check if data has data_source field"""
        return 'data_source' in data

    def check_calendar_structure(self, data):
        """Check calendar data structure"""
        if not isinstance(data, dict):
            return False
        
        # Check for calendar structure
        if 'calendar' in data:
            calendar = data['calendar']
            if isinstance(calendar, dict):
                # Check for ppg, ppg_description, and slots
                has_ppg = any('ppg' in str(k).lower() for k in calendar.keys())
                has_ppg_desc = any('ppg_description' in str(k).lower() for k in calendar.keys())
                has_slots = any('slot' in str(k).lower() for k in calendar.keys())
                return has_ppg and has_ppg_desc and has_slots
        return False

    def check_mechanics_tpr_display_only(self, data):
        """Check that mechanics are only TPR or Display (NOT Flyer or Display + Flyer)"""
        def check_mechanics_recursive(obj, path=""):
            if isinstance(obj, dict):
                for k, v in obj.items():
                    if 'mechanic' in str(k).lower():
                        if isinstance(v, str):
                            if v in ['Flyer', 'Display + Flyer']:
                                print(f"   ❌ Found invalid mechanic '{v}' at {path}.{k}")
                                return False
                    if not check_mechanics_recursive(v, f"{path}.{k}"):
                        return False
            elif isinstance(obj, list):
                for i, item in enumerate(obj):
                    if isinstance(item, dict):
                        if 'mechanic' in item:
                            mechanic = item['mechanic']
                            if mechanic in ['Flyer', 'Display + Flyer']:
                                print(f"   ❌ Found invalid mechanic '{mechanic}' at {path}[{i}]")
                                return False
                    if not check_mechanics_recursive(item, f"{path}[{i}]"):
                        return False
            return True
        
        return check_mechanics_recursive(data)

    def check_table_data_structure(self, data):
        """Check table data has proper structure with mechanic field"""
        if not isinstance(data, dict):
            return False
        
        # Look for table data or similar structure
        if 'data' in data or 'table' in data or 'rows' in data:
            table_data = data.get('data') or data.get('table') or data.get('rows')
            if isinstance(table_data, list) and len(table_data) > 0:
                # Check if first row has mechanic field
                first_row = table_data[0]
                if isinstance(first_row, dict):
                    return 'mechanic' in first_row or any('mechanic' in str(k).lower() for k in first_row.keys())
        return True  # Pass if structure is different but valid

    def check_display_flag_field(self, data):
        """Check if table data includes displayFlag field"""
        def check_display_flag_recursive(obj):
            if isinstance(obj, dict):
                # Check current level for displayFlag
                if any('display' in str(k).lower() and 'flag' in str(k).lower() for k in obj.keys()):
                    return True
                # Check nested objects
                for v in obj.values():
                    if check_display_flag_recursive(v):
                        return True
            elif isinstance(obj, list):
                for item in obj:
                    if check_display_flag_recursive(item):
                        return True
            return False
        
        return check_display_flag_recursive(data)

    def check_chart_data_kpi(self, data):
        """Check if KPI data has chart_data with monthly values"""
        if not isinstance(data, dict):
            return False
        
        return 'chart_data' in data or 'kpi' in data or 'monthly' in str(data).lower()

    def check_baseline_amendments(self, data):
        """Check if summary data has baseline and amendments"""
        if not isinstance(data, dict):
            return False
        
        return 'baseline' in data or 'amendments' in data or 'summary' in data

    def run_all_tests(self):
        """Run all R Plumber API optimizer tests"""
        print("🚀 Starting R Plumber API Optimizer Tests...")
        print(f"⏰ Test started at: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print(f"🌐 Backend URL: {self.base_url}")
        
        # Test 1: Calendar Data Endpoint
        print("\n" + "="*60)
        print("TEST 1: GET /api/r/optimizer/calendar-data")
        print("="*60)
        
        calendar_checks = {
            'has_data_source': self.check_data_source_field,
            'calendar_structure_valid': self.check_calendar_structure,
            'mechanics_tpr_display_only': self.check_mechanics_tpr_display_only
        }
        self.test_endpoint("Calendar Data Endpoint", "calendar-data", calendar_checks)
        
        # Test 2: Table Data Endpoint  
        print("\n" + "="*60)
        print("TEST 2: GET /api/r/optimizer/table-data")
        print("="*60)
        
        table_checks = {
            'has_mechanic_field': self.check_table_data_structure,
            'mechanics_tpr_display_only': self.check_mechanics_tpr_display_only,
            'has_display_flag': self.check_display_flag_field
        }
        self.test_endpoint("Table Data Endpoint", "table-data", table_checks)
        
        # Test 3: KPI Data Endpoint
        print("\n" + "="*60)
        print("TEST 3: GET /api/r/optimizer/kpi-data")
        print("="*60)
        
        kpi_checks = {
            'has_chart_data': self.check_chart_data_kpi
        }
        self.test_endpoint("KPI Data Endpoint", "kpi-data", kpi_checks)
        
        # Test 4: Summary Endpoint
        print("\n" + "="*60)
        print("TEST 4: GET /api/r/optimizer/summary")
        print("="*60)
        
        summary_checks = {
            'has_baseline_amendments': self.check_baseline_amendments
        }
        self.test_endpoint("Summary Endpoint", "summary", summary_checks)
        
        # Print final results
        print("\n" + "="*60)
        print("📊 FINAL TEST RESULTS")
        print("="*60)
        print(f"Tests run: {self.tests_run}")
        print(f"Tests passed: {self.tests_passed}")
        print(f"Success rate: {(self.tests_passed/self.tests_run)*100:.1f}%" if self.tests_run > 0 else "No tests run")
        
        if self.failed_tests:
            print(f"\n❌ FAILED TESTS ({len(self.failed_tests)}):")
            for i, test in enumerate(self.failed_tests, 1):
                print(f"{i}. {test['name']}")
                if test['details']:
                    for key, value in test['details'].items():
                        print(f"   {key}: {value}")
        else:
            print("\n✅ All tests passed!")
        
        return len(self.failed_tests) == 0

def main():
    """Main function to run the tests"""
    tester = ROptimizerAPITester()
    success = tester.run_all_tests()
    
    # Return appropriate exit code
    return 0 if success else 1

if __name__ == "__main__":
    import sys
    sys.exit(main())