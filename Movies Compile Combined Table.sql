/* Compile combined table */

SELECT m.rotten_tomatoes_link
, m.movie_title
, m.genres
, m.original_release_date
, m.runtime
, m.audience_rating
, COUNT(r.rotten_tomatoes_link) AS num_reviews
, SUM(CASE WHEN r.review_type = 'Fresh' THEN 1
	 WHEN r.review_type = 'Rotten' THEN 0
	 END)/CAST(COUNT(r.review_type) AS DECIMAL) * 100 AS percent_fresh
, SUM(r.normalized_score)/COUNT(r.normalized_score)  AS avg_score
FROM movies m
INNER JOIN reviews r
ON m.rotten_tomatoes_link = r.rotten_tomatoes_link
WHERE r.normalized_score IS NOT NULL
GROUP BY m.rotten_tomatoes_link
, m.movie_title
, m.genres
, m.original_release_date
, m.runtime
, m.audience_rating