"""
Backend API Tests for PromoGen R-Shiny to React/FastAPI Migration
Tests all new promo endpoints, user management, and core functionality
"""
import pytest
import requests
import os

BASE_URL = os.environ.get('REACT_APP_BACKEND_URL', '').rstrip('/')

# Test credentials
ADMIN_EMAIL = "admin@promogen.com"
ADMIN_PASSWORD = "admin123"


class TestHealthAndAuth:
    """Health check and authentication tests"""
    
    def test_health_endpoint(self):
        """Test health endpoint is accessible"""
        response = requests.get(f"{BASE_URL}/api/health")
        assert response.status_code == 200
        data = response.json()
        assert data["status"] == "healthy"
        print("✓ Health endpoint working")
    
    def test_root_endpoint(self):
        """Test root API endpoint"""
        response = requests.get(f"{BASE_URL}/api/")
        assert response.status_code == 200
        data = response.json()
        assert "PromoGen" in data["message"]
        print("✓ Root endpoint working")
    
    def test_login_success(self):
        """Test admin login with valid credentials"""
        response = requests.post(f"{BASE_URL}/api/auth/login", json={
            "email": ADMIN_EMAIL,
            "password": ADMIN_PASSWORD
        })
        assert response.status_code == 200
        data = response.json()
        assert "access_token" in data
        assert data["token_type"] == "bearer"
        print(f"✓ Login successful for {ADMIN_EMAIL}")
    
    def test_login_invalid_credentials(self):
        """Test login with invalid credentials"""
        response = requests.post(f"{BASE_URL}/api/auth/login", json={
            "email": "wrong@example.com",
            "password": "wrongpass"
        })
        assert response.status_code == 401
        print("✓ Invalid login correctly rejected")


@pytest.fixture
def auth_token():
    """Get authentication token for admin user"""
    response = requests.post(f"{BASE_URL}/api/auth/login", json={
        "email": ADMIN_EMAIL,
        "password": ADMIN_PASSWORD
    })
    if response.status_code == 200:
        return response.json().get("access_token")
    pytest.skip("Authentication failed - skipping authenticated tests")


@pytest.fixture
def auth_headers(auth_token):
    """Get headers with auth token"""
    return {
        "Authorization": f"Bearer {auth_token}",
        "Content-Type": "application/json"
    }


class TestUserManagement:
    """User management endpoint tests including Edit User"""
    
    def test_list_users(self, auth_headers):
        """Test listing all users"""
        response = requests.get(f"{BASE_URL}/api/users/", headers=auth_headers)
        assert response.status_code == 200
        data = response.json()
        assert isinstance(data, list)
        assert len(data) >= 1  # At least admin user
        print(f"✓ Listed {len(data)} users")
    
    def test_get_user_by_id(self, auth_headers):
        """Test getting user by ID"""
        # First get list of users
        list_response = requests.get(f"{BASE_URL}/api/users/", headers=auth_headers)
        users = list_response.json()
        if users:
            user_id = users[0]["id"]
            response = requests.get(f"{BASE_URL}/api/users/{user_id}", headers=auth_headers)
            assert response.status_code == 200
            data = response.json()
            assert data["id"] == user_id
            print(f"✓ Got user by ID: {user_id}")
    
    def test_update_user_name(self, auth_headers):
        """Test updating user name via PUT endpoint"""
        # Get list of users
        list_response = requests.get(f"{BASE_URL}/api/users/", headers=auth_headers)
        users = list_response.json()
        
        # Find a non-admin user or use admin
        target_user = None
        for user in users:
            if user["email"] != ADMIN_EMAIL:
                target_user = user
                break
        
        if not target_user and users:
            target_user = users[0]
        
        if target_user:
            original_name = target_user["name"]
            new_name = f"TEST_{original_name}"
            
            # Update user
            response = requests.put(
                f"{BASE_URL}/api/users/{target_user['id']}", 
                headers=auth_headers,
                json={"name": new_name}
            )
            assert response.status_code == 200
            data = response.json()
            assert data["name"] == new_name
            print(f"✓ Updated user name to: {new_name}")
            
            # Revert change
            requests.put(
                f"{BASE_URL}/api/users/{target_user['id']}", 
                headers=auth_headers,
                json={"name": original_name}
            )
    
    def test_update_user_role(self, auth_headers):
        """Test updating user role"""
        list_response = requests.get(f"{BASE_URL}/api/users/", headers=auth_headers)
        users = list_response.json()
        
        # Find a non-admin user
        target_user = None
        for user in users:
            if user["role"] != "admin":
                target_user = user
                break
        
        if target_user:
            original_role = target_user["role"]
            new_role = "viewer" if original_role == "user" else "user"
            
            response = requests.put(
                f"{BASE_URL}/api/users/{target_user['id']}", 
                headers=auth_headers,
                json={"role": new_role}
            )
            assert response.status_code == 200
            data = response.json()
            assert data["role"] == new_role
            print(f"✓ Updated user role to: {new_role}")
            
            # Revert
            requests.put(
                f"{BASE_URL}/api/users/{target_user['id']}", 
                headers=auth_headers,
                json={"role": original_role}
            )
        else:
            print("⚠ No non-admin user found to test role update")
    
    def test_update_user_invalid_role(self, auth_headers):
        """Test updating user with invalid role"""
        list_response = requests.get(f"{BASE_URL}/api/users/", headers=auth_headers)
        users = list_response.json()
        
        if users:
            response = requests.put(
                f"{BASE_URL}/api/users/{users[0]['id']}", 
                headers=auth_headers,
                json={"role": "invalid_role"}
            )
            assert response.status_code == 400
            print("✓ Invalid role correctly rejected")
    
    def test_toggle_user_status(self, auth_headers):
        """Test toggling user active status"""
        list_response = requests.get(f"{BASE_URL}/api/users/", headers=auth_headers)
        users = list_response.json()
        
        # Find a non-admin user
        target_user = None
        for user in users:
            if user["email"] != ADMIN_EMAIL:
                target_user = user
                break
        
        if target_user:
            response = requests.patch(
                f"{BASE_URL}/api/users/{target_user['id']}/toggle-status", 
                headers=auth_headers
            )
            assert response.status_code == 200
            data = response.json()
            assert "is_active" in data
            print(f"✓ Toggled user status to: {data['is_active']}")
            
            # Toggle back
            requests.patch(
                f"{BASE_URL}/api/users/{target_user['id']}/toggle-status", 
                headers=auth_headers
            )
        else:
            print("⚠ No non-admin user found to test status toggle")


class TestPromoFilters:
    """Promo filter endpoint tests"""
    
    def test_get_filters(self, auth_headers):
        """Test getting filter options"""
        response = requests.get(f"{BASE_URL}/api/promo/filters", headers=auth_headers)
        assert response.status_code == 200
        data = response.json()
        
        # Verify all filter categories exist
        assert "countries" in data
        assert "customers" in data
        assert "categories" in data
        assert "brands" in data
        assert "formats" in data
        assert "ppgs" in data
        assert "manufacturers" in data
        
        # Verify data
        assert "UAE" in data["countries"]
        assert len(data["brands"]) > 0
        print(f"✓ Got filters: {len(data['brands'])} brands, {len(data['ppgs'])} PPGs")


class TestExecutiveSummary:
    """Executive Summary page API tests"""
    
    def test_get_kpis(self, auth_headers):
        """Test getting KPI data"""
        response = requests.get(f"{BASE_URL}/api/promo/kpis", headers=auth_headers)
        assert response.status_code == 200
        data = response.json()
        
        assert isinstance(data, list)
        assert len(data) >= 6  # Should have 6 KPIs
        
        # Verify KPI structure
        for kpi in data:
            assert "label" in kpi
            assert "value" in kpi
            assert "change" in kpi
            assert "trend" in kpi
        print(f"✓ Got {len(data)} KPIs")
    
    def test_get_value_share(self, auth_headers):
        """Test getting value share data"""
        response = requests.get(f"{BASE_URL}/api/promo/value-share", headers=auth_headers)
        assert response.status_code == 200
        data = response.json()
        
        assert isinstance(data, list)
        assert len(data) > 0
        print(f"✓ Got {len(data)} value share entries")
    
    def test_get_volume_trend(self, auth_headers):
        """Test getting volume trend data"""
        response = requests.get(f"{BASE_URL}/api/promo/volume-trend", headers=auth_headers)
        assert response.status_code == 200
        data = response.json()
        
        assert isinstance(data, list)
        assert len(data) == 52  # 52 weeks
        print(f"✓ Got {len(data)} weeks of volume trend data")
    
    def test_get_volume_decomposition(self, auth_headers):
        """Test getting volume decomposition data"""
        response = requests.get(f"{BASE_URL}/api/promo/volume-decomposition", headers=auth_headers)
        assert response.status_code == 200
        data = response.json()
        
        assert isinstance(data, list)
        assert len(data) >= 10  # Should have 10 metrics
        
        # Verify structure
        for item in data:
            assert "metrics" in item
            assert "cy_value" in item
            assert "ya_value" in item
            assert "abs_change" in item
            assert "pct_change" in item
        print(f"✓ Got {len(data)} volume decomposition metrics")
    
    def test_get_data_table(self, auth_headers):
        """Test getting SKU data table"""
        response = requests.get(f"{BASE_URL}/api/promo/data-table", headers=auth_headers)
        assert response.status_code == 200
        data = response.json()
        
        assert isinstance(data, list)
        assert len(data) == 20  # Should have 20 SKUs
        
        # Verify structure
        for item in data:
            assert "sku" in item
            assert "ppg" in item
            assert "brand" in item
            assert "volume_sales" in item
        print(f"✓ Got {len(data)} SKU records")
    
    def test_get_promo_split(self, auth_headers):
        """Test getting promo split data"""
        response = requests.get(f"{BASE_URL}/api/promo/promo-split", headers=auth_headers)
        assert response.status_code == 200
        data = response.json()
        
        assert isinstance(data, list)
        assert len(data) > 0
        print(f"✓ Got {len(data)} promo split entries")
    
    def test_get_promo_wow(self, auth_headers):
        """Test getting week-over-week promo data"""
        response = requests.get(f"{BASE_URL}/api/promo/promo-wow", headers=auth_headers)
        assert response.status_code == 200
        data = response.json()
        
        assert isinstance(data, list)
        assert len(data) == 12  # 12 weeks
        print(f"✓ Got {len(data)} weeks of WoW data")


class TestPromotionEffectiveness:
    """Promotion Effectiveness page API tests"""
    
    def test_get_roi_analysis(self, auth_headers):
        """Test getting ROI analysis data"""
        response = requests.get(f"{BASE_URL}/api/promo/roi-analysis", headers=auth_headers)
        assert response.status_code == 200
        data = response.json()
        
        assert isinstance(data, list)
        assert len(data) > 0
        
        # Verify structure
        for item in data:
            assert "name" in item
            assert "roi" in item
            assert "investment" in item
            assert "incremental_sales" in item
            assert "event_type" in item
        print(f"✓ Got {len(data)} ROI analysis entries")
    
    def test_get_promo_calendar(self, auth_headers):
        """Test getting promotion calendar"""
        response = requests.get(f"{BASE_URL}/api/promo/calendar", headers=auth_headers)
        assert response.status_code == 200
        data = response.json()
        
        assert isinstance(data, list)
        assert len(data) > 0
        
        # Verify structure
        for item in data:
            assert "id" in item
            assert "ppg" in item
            assert "brand" in item
            assert "event_type" in item
            assert "week" in item
            assert "discount" in item
            assert "roi" in item
        print(f"✓ Got {len(data)} calendar events")
    
    def test_get_promo_fact_table(self, auth_headers):
        """Test getting promo fact table"""
        response = requests.get(f"{BASE_URL}/api/promo/promo-fact-table", headers=auth_headers)
        assert response.status_code == 200
        data = response.json()
        
        assert isinstance(data, list)
        assert len(data) > 0
        
        # Verify structure
        for item in data:
            assert "ppg" in item
            assert "brand" in item
            assert "events" in item
            assert "roi_gm" in item
        print(f"✓ Got {len(data)} fact table entries")


class TestOptimizer:
    """Optimizer page API tests"""
    
    def test_get_optimizer_constraints(self, auth_headers):
        """Test getting optimizer constraints"""
        response = requests.get(f"{BASE_URL}/api/promo/optimizer/constraints", headers=auth_headers)
        assert response.status_code == 200
        data = response.json()
        
        assert isinstance(data, list)
        assert len(data) >= 6  # Should have 6 constraints
        
        # Verify structure
        for item in data:
            assert "kpi" in item
            assert "current_value" in item
        print(f"✓ Got {len(data)} optimizer constraints")
    
    def test_run_optimization(self, auth_headers):
        """Test running optimization"""
        response = requests.post(
            f"{BASE_URL}/api/promo/optimizer/run",
            headers=auth_headers,
            json={
                "goal": "Scan Net Revenue",
                "sign": "Max",
                "start_date": "2024-01-01",
                "end_date": "2024-12-31",
                "max_display_slots": 6,
                "roi_type": "Incremental GM ROI"
            }
        )
        assert response.status_code == 200
        data = response.json()
        
        assert data["status"] == "completed"
        assert "improvement_pct" in data
        print(f"✓ Optimization completed with {data['improvement_pct']}% improvement")


class TestOptimizationOutputs:
    """Optimization Outputs page API tests"""
    
    def test_get_optimization_results(self, auth_headers):
        """Test getting optimization results"""
        response = requests.get(f"{BASE_URL}/api/promo/optimizer/results", headers=auth_headers)
        assert response.status_code == 200
        data = response.json()
        
        assert isinstance(data, list)
        assert len(data) > 0
        
        # Verify structure
        for item in data:
            assert "ppg" in item
            assert "ly_volume" in item
            assert "lsm_volume" in item
            assert "unconstrained_volume" in item
            assert "ly_roi" in item
            assert "lsm_roi" in item
            assert "unconstrained_roi" in item
        print(f"✓ Got {len(data)} optimization results")
    
    def test_get_scenario_comparison(self, auth_headers):
        """Test getting scenario comparison"""
        response = requests.get(f"{BASE_URL}/api/promo/scenarios/comparison", headers=auth_headers)
        assert response.status_code == 200
        data = response.json()
        
        assert isinstance(data, list)
        assert len(data) >= 4  # Should have 4 scenarios
        
        # Verify structure
        for item in data:
            assert "scenario" in item
            assert "net_revenue" in item
            assert "gross_margin" in item
            assert "roi" in item
        print(f"✓ Got {len(data)} scenarios for comparison")


class TestEventSimulator:
    """Event Simulator page API tests"""
    
    def test_get_simulator_events(self, auth_headers):
        """Test getting simulator events"""
        response = requests.get(f"{BASE_URL}/api/promo/simulator/events", headers=auth_headers)
        assert response.status_code == 200
        data = response.json()
        
        assert isinstance(data, list)
        assert len(data) > 0
        
        # Verify structure
        for item in data:
            assert "id" in item
            assert "ppg" in item
            assert "current_event" in item
            assert "alt_event" in item
            assert "current_roi" in item
            assert "alt_roi" in item
        print(f"✓ Got {len(data)} simulator events")
    
    def test_get_simulator_events_filtered(self, auth_headers):
        """Test getting simulator events with PPG filter"""
        response = requests.get(
            f"{BASE_URL}/api/promo/simulator/events",
            headers=auth_headers,
            params={"ppg": "PPG-001"}
        )
        assert response.status_code == 200
        data = response.json()
        
        # All results should be for PPG-001
        for item in data:
            assert item["ppg"] == "PPG-001"
        print(f"✓ Got {len(data)} filtered simulator events")
    
    def test_run_simulation(self, auth_headers):
        """Test running simulation"""
        response = requests.post(
            f"{BASE_URL}/api/promo/simulator/run",
            headers=auth_headers,
            params={
                "event_id": "sim-PPG-001-1",
                "alt_event": "Display",
                "alt_discount": 25.0
            }
        )
        assert response.status_code == 200
        data = response.json()
        
        assert data["status"] == "completed"
        assert "original_roi" in data
        assert "simulated_roi" in data
        assert "volume_change_pct" in data
        print(f"✓ Simulation completed: ROI {data['original_roi']} -> {data['simulated_roi']}")


class TestKAMCockpit:
    """KAM Cockpit page API tests"""
    
    def test_get_kam_rb_metrics(self, auth_headers):
        """Test getting KAM RB P&L metrics"""
        response = requests.get(f"{BASE_URL}/api/promo/kam/rb-metrics", headers=auth_headers)
        assert response.status_code == 200
        data = response.json()
        
        assert isinstance(data, list)
        assert len(data) >= 6  # Should have 6 metrics
        
        # Verify structure
        for item in data:
            assert "metric" in item
            assert "ly_value" in item
            assert "cy_value" in item
            assert "change_pct" in item
        print(f"✓ Got {len(data)} RB P&L metrics")
    
    def test_get_kam_customer_metrics(self, auth_headers):
        """Test getting KAM Customer P&L metrics"""
        response = requests.get(f"{BASE_URL}/api/promo/kam/customer-metrics", headers=auth_headers)
        assert response.status_code == 200
        data = response.json()
        
        assert isinstance(data, list)
        assert len(data) >= 5  # Should have 5 metrics
        
        # Verify structure
        for item in data:
            assert "metric" in item
            assert "ly_value" in item
            assert "cy_value" in item
            assert "change_pct" in item
        print(f"✓ Got {len(data)} Customer P&L metrics")


class TestSettings:
    """Settings API tests"""
    
    def test_get_branding(self, auth_headers):
        """Test getting branding settings"""
        response = requests.get(f"{BASE_URL}/api/settings/branding", headers=auth_headers)
        assert response.status_code == 200
        data = response.json()
        
        assert "app_name" in data
        print(f"✓ Got branding settings: {data.get('app_name', 'N/A')}")


if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])
