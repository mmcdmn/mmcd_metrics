"""Temp script to explore employee_list and check for claims table."""
import psycopg2

conn = psycopg2.connect(
    host='rds-readonly.mmcd.org', port=5432,
    user='mmcd_read', password='mmcd2012', dbname='mmcd_data'
)
cur = conn.cursor()

# 1. Emp types
cur.execute("SELECT emp_type, count(*) FROM employee_list WHERE active = true GROUP BY emp_type ORDER BY count(*) DESC")
print("=== EMPLOYEE TYPES ===")
for r in cur.fetchall():
    print(f"  {r[0]}: {r[1]}")

# 2. Count employees with fieldsuper
cur.execute("SELECT count(*) FROM employee_list WHERE active = true AND fieldsuper IS NOT NULL")
print(f"\nEmployees with fieldsuper: {cur.fetchone()[0]}")

# 3. Sample non-FOS employees under a specific FOS
cur.execute("""
    SELECT e.emp_num, e.shortname, e.emp_type, e.facility, e.fieldsuper, f.shortname as fos_name
    FROM employee_list e
    LEFT JOIN employee_list f ON e.fieldsuper = f.emp_num AND f.emp_type = 'FieldSuper'
    WHERE e.active = true AND e.facility = 'N'
    ORDER BY e.fieldsuper, e.lastname
    LIMIT 25
""")
print("\n=== NORTH FACILITY EMPLOYEES ===")
for r in cur.fetchall():
    print(f"  {r[0]} {r[1]:15s} type={r[2]:12s} fac={r[3]} super={r[4]} ({r[5]})")

# 4. Does claims table exist?
cur.execute("SELECT EXISTS(SELECT 1 FROM information_schema.tables WHERE table_name = 'air_checklist_claims')")
print(f"\nClaims table exists: {cur.fetchone()[0]}")

# 5. Check if DB user has CREATE TABLE permission
cur.execute("SELECT current_user, session_user")
print(f"Current user: {cur.fetchone()}")

conn.close()
