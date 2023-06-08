/* Set letter grade scores to corresponding numeric scores out of 100 */

UPDATE reviews
SET normalized_score = CASE 
	WHEN review_score = 'A+' THEN 97.5
	WHEN review_score = 'A' THEN 95
	WHEN review_score = 'A-' THEN 92.5
	WHEN review_score = 'B+' THEN 87.5
	WHEN review_score = 'B' THEN 85
	WHEN review_score = 'B-' THEN 82.5
	WHEN review_score = 'C+' THEN 77.5
	WHEN review_score = 'C' THEN 75
	WHEN review_score = 'C-' THEN 72.5
	WHEN review_score = 'D+' THEN 67.5
	WHEN review_score = 'D' THEN 65
	WHEN review_score = 'D-' THEN 62.5
	WHEN review_score = 'F' THEN 50
	END
WHERE review_score  ~* '[a-z]'