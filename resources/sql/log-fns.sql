-- name: get-log-group-counts*
-- returns: :array-hash
SELECT name, read_count
FROM blog.loggroup_reads 
JOIN blog.log_group ON group_id = blog.log_group.ID
WHERE user_id = $1;

-- name: upsert-readcount* @execute
INSERT INTO blog.loggroup_reads
SELECT g.id, $2, $1
FROM blog.log_group g 
WHERE g.name = $3
ON CONFLICT( group_id, user_id) DO
UPDATE SET read_count = excluded.read_count;
