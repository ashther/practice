import sqlite3
from collections import defaultdict
from datetime import datetime
import progressbar
# from tqdm import tqdm
import networkx as nx


con = sqlite3.connect('D:/Documents/R Scripts/oracle/db/card.sqlite')
sql = """
    SELECT accnum,
          devicenum,
          dealtime
   FROM [transaction]
   WHERE recflag = 1 AND 
         isred = 0 AND 
         devicenum <> 0 AND 
         businessnum <> 100058 AND 
         -- dealtime > datetime(datetime('now', '-180 days'), 'localtime') AND 
         accnum IN (
           SELECT accnum
           FROM account
           WHERE acctype = 1 AND 
                 accname <> '内部账户' AND 
                 substr(percode, 1, 1) IN ('1', '2', '3') 
         )
   ORDER BY devicenum,
            dealtime;
"""
# df = pd.read_sql_query(sql, con)
consor = con.execute(sql)
df = consor.fetchall()
consor.close()

# con = sqlite3.connect('D:/Documents/R Scripts/db_userProfile/db.sqlite')
sql = """
    SELECT accnum,
           accdepid AS major_id
    FROM account
    WHERE acctype = 1 AND 
          accname <> '内部账户' AND 
          substr(percode, 1, 1) IN ('1', '2', '3');
"""
consor = con.execute(sql)
accnum_major = consor.fetchall()
consor.close()
accnum_major = {x[0]: x[1] for x in accnum_major}

sql = """
    SELECT DISTINCT accdepid AS major_id,
                    parentid AS college_id
    FROM acc_dep
    WHERE parentid <> -1;
"""
consor = con.execute(sql)
major_college = consor.fetchall()
consor.close()
con.close()
major_college = {x[0]: x[1] for x in major_college}

dt = defaultdict(int)
fmt = '%Y/%m/%d %H:%M:%S'
with progressbar.ProgressBar(max_value=len(df)) as bar:
    for i, transaction in enumerate(df):
        if (transaction[0] != df[i - 1][0]) and \
                (transaction[1] == df[i - 1][1]) and \
                ((datetime.strptime(transaction[2], fmt) -
                  datetime.strptime(df[i - 1][2], fmt)).seconds <= 60):
            dt[tuple(sorted((transaction[0], df[i - 1][0])))] += 1
        bar.update(i)

del df

dt = [(*k, v) for k, v in dt.items()]
# dt = pd.read_csv('graph/edgelist.csv')

graph_user = nx.Graph()
graph_user.add_weighted_edges_from(dt)
nx.write_gpickle(graph_user, 'graph/graphUser.gpickle')

dt_major = defaultdict(int)
for x in dt:
    fr = accnum_major.get(x[0])
    to = accnum_major.get(x[1])
    if (fr is not None) and (to is not None) and (fr != to):
        dt_major[(fr, to)] += x[2]
dt_major = [(*k, v) for k, v in dt_major.items()]
graph_major = nx.Graph()
graph_major.add_weighted_edges_from(dt_major)
nx.write_gpickle(graph_major, 'graph/graphMajor.pkl')

dt_college = defaultdict(int)
for x in dt_major:
    fr = major_college.get(x[0])
    to = major_college.get(x[1])
    if (fr is not None) and (to is not None) and (fr != to):
        dt_college[(fr, to)] += x[2]
dt_college = [(*k, v) for k, v in dt_college.items()]
graph_college = nx.Graph()
graph_college.add_weighted_edges_from(dt_college)
nx.write_gpickle(graph_college, 'graph/graphCollege.pkl')


# subgraph for different college
college_major = defaultdict(list)
for k, v in major_college.items():
    college_major[v].append(k)

college_subgraph = []
for k, v in college_major:
    try:
        pass
    except Exception as e:
        print(e)