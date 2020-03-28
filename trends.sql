SELECT
place,
date_trunc('WEEK', asof),
max(confirmed) as confirmed,
max(deaths) as deaths,
max(recovered) as recovered
FROM reports
GROUP BY 1, 2
ORDER BY 1, 2;

SELECT
r1.place,
r2.asof,
max(r1.confirmed),
max(r1.deaths),
FROM reports r1
INNER JOIN reports r2
ON r1.place = r2.place AND
r1.asof + 7 = r2.asof
GROUP BY 1, 2
ORDER BY 1, 2;

CREATE VIEW new_last_week AS
SELECT
r1.place,
r1.asof,
max(r1.confirmed) as confirmed,
sum(r1.confirmed) - sum(r2.confirmed) as new_confirmed,
max(r1.deaths) as deaths,
sum(r1.deaths) - sum(r2.deaths) as new_deaths,
max(r1.recovered) as recovered,
sum(r1.recovered) - sum(r2.recovered) as new_recovered
FROM reports r1
INNER JOIN reports r2
ON r1.place = r2.place AND
r1.asof - 7 = r2.asof
GROUP BY 1, 2
ORDER BY 1, 2;
