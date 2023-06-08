/* Check the proportion of different kinds of review scores */

SELECT SUM(CASE WHEN review_score IS NULL THEN 1
	 		ELSE 0
	 		END)/CAST(COUNT(*) AS DECIMAL)  AS no_score
	   , SUM(CASE WHEN review_score  ~* '[a-z]' THEN 1
		   ELSE 0
		   END)/CAST(COUNT(*) AS DECIMAL) AS letter_score
	   , SUM(CASE WHEN review_score LIKE '%/%' THEN 1
			ELSE 0
			END)/ CAST(COUNT(*) AS DECIMAL) AS numeric_score
FROM reviews