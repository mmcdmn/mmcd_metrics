"""Explore employee-to-FOS mapping correctly."""
import psycopg2

conn = psycopg2.connect(
    host='rds-readonly.mmcd.org', port=5432,
    user='mmcd_read', password='mmcd2012', dbname='mmcd_data'
)
cur = conn.cursor()

# Get employees with their single fieldsuper, joined to get the FOS name
cur.execute("""
    SELECT e.emp_num, e.shortname, e.emp_type, e.facility, e.fieldsuper,
           f.shortname as fos_name
    FROM employee_list e
    LEFT JOIN employee_list f ON e.fieldsuper = f.emp_num 
        AND f.active = true AND f.emp_type = 'FieldSuper'
    WHERE e.active = true 
      AND e.facility = 'N'
    ORDER BY e.fieldsuper, e.emp_type, e.lastname
""")
print("=== NORTH FACILITY (with FOS name) ===")
for r in cur.fetchall():
    print(f"  emp={r[0]} {r[1]:15s} type={r[2]:12s} fac={r[3]} super={r[4]} fos_name={r[5]}")

# Let's see what the FieldSuper join issue is
cur.execute("""
    SELECT emp_num, shortname, count(*) 
    FROM employee_list 
    WHERE emp_type = 'FieldSuper' AND active = true 
    GROUP BY emp_num, shortname
    HAVING count(*) > 1
""")
print("\n=== Duplicate FieldSuper emp_nums ===")
for r in cur.fetchall():
    print(f"  {r}")

# Maybe there are historical records (active=true but same emp_num).
# Let's see all records for emp_num '0203'
cur.execute("""
    SELECT emp_num, shortname, firstname, lastname, active, date_start, date_end, emp_type, pkey
    FROM employee_list 
    WHERE emp_num = '0203'
    ORDER BY date_start
""")
print("\n=== All records for emp_num 0203 ===")
for r in cur.fetchall():
    print(f"  {r}")

# Get distinct active employees (non-FOS, non-RegOpMgr) for the "crew" dropdown
cur.execute("""
    SELECT DISTINCT e.emp_num, e.shortname, e.emp_type, e.facility, e.fieldsuper
    FROM employee_list e
    WHERE e.active = true 
      AND e.fieldsuper IS NOT NULL
      AND e.emp_type NOT IN ('Pilot', 'Insp-Recpt', 'Insp-Lab')
    ORDER BY e.facility, e.shortname
""")
print("\n=== Field employees (for claim dropdown) ===")
results = cur.fetchall()
print(f"Total: {len(results)}")
for r in results[:20]:
    print(f"  emp={r[0]} {r[1]:15s} type={r[2]:12s} fac={r[3]} super={r[4]}")

conn.close()
