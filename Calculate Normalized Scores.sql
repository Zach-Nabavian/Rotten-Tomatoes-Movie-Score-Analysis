/* Split numerical scores into their individual scores and
highest possible scores, then normalize all scores out of 10 */

ALTER TABLE reviews
ADD COLUMN numeric_score FLOAT;

UPDATE reviews
SET numeric_score = CAST(SPLIT_PART(review_score, '/', 1) AS FLOAT)
WHERE review_score LIKE '%/%'; 

ALTER TABLE reviews
ADD COLUMN max_score FLOAT;

UPDATE reviews
SET max_score = CAST(SPLIT_PART(review_score, '/', 2) AS FLOAT)
WHERE review_score LIKE '%/%';

ALTER TABLE reviews
ADD COLUMN normalized_score FLOAT;

UPDATE reviews
SET normalized_score = numeric_score/max_score * 100
WHERE review_score LIKE '%/%';

SELECT review_score
, numeric_score
, max_score
, normalized_score
FROM reviews 


