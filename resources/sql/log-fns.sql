-- name: get-log-group-counts*
-- returns: :array-hash
SELECT name, read_count
FROM blog.loggroup_reads 
JOIN blog.log_group ON group_id = blog.log_group.ID
WHERE user_id = $1;

-- name: group-id*
-- count: single
SELECT id from blog.log_group where name = $1;

-- name: update-readcount* @execute
UPDATE blog.loggroup_reads r
SET read_count = $1
FROM blog.log_group g 
WHERE r.group_id = g.id AND r.user_id = $2 AND g.name = $3;

-- name: insert-readcount* @execute
insert into blog.loggroup_reads
select g.id, $2, $1
from blog.log_group g 
WHERE g.name = $3;

-- name: readcount-exists?
-- count: single
select exists (select *
       	       from blog.loggroup_reads
	       JOIN blog.log_group gr ON group_id = gr.ID
	       where user_id = $1 and gr.name = $2);
