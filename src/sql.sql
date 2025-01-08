select * from covid_19_deaths;

select * from covid_19_deaths;

CREATE TABLE Total_Covid_deaths AS
SELECT LA_name, LA_code, SUM(Total_deaths) AS Total_deaths
FROM covid_19_deaths
GROUP BY LA_name, LA_code;

SELECT * 
FROM Total_Covid_deaths, nomis_data
WHERE Total_Covid_deaths.LA_code = nomis_data.LA_code;
