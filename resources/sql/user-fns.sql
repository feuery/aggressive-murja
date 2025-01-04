-- name: get-user-by-id*
-- returns: :array-hash

SELECT
  u.id,
  u.username,
  u.nickname,
  u.img_location,
  u.password,
  json_agg(DISTINCT perm.action) "permissions"
FROM
  blog.users u
  JOIN blog.groupmapping gm ON u.id = gm.userid
  JOIN blog.grouppermissions gp ON gp.groupid = gm.groupid
  JOIN blog.permission perm ON gp.permissionid = perm.id
WHERE
  u.id = $1
GROUP BY
  u.id;

-- name: query-users*
-- returns: :array-hash
SELECT u.Username, u.Nickname, u.Img_location, ug.Name as "primary-group-name", gm.PrimaryGroup, u.ID as userid, json_agg(DISTINCT perm.action) as "permissions"
FROM blog.Users u
JOIN blog.GroupMapping gm ON u.ID = gm.UserID
JOIN blog.UserGroup ug ON ug.ID = gm.GroupID
JOIN blog.grouppermissions gp ON gp.groupid = gm.groupid
JOIN blog.permission perm ON perm.id = gp.permissionid
WHERE u.Username = $1 AND u.Password = $2
GROUP BY u.Username, u.Nickname, u.Img_location, ug.Name, gm.PrimaryGroup, u.ID;

-- name: query-user-for-session
-- returns: :array-hash 
SELECT u.Username, u.Nickname, u.Img_location, ug.Name as "primary-group-name", gm.PrimaryGroup, u.ID as userid, json_agg(DISTINCT perm.action) as "permissions"
FROM blog.Users u
JOIN blog.GroupMapping gm ON u.ID = gm.UserID
JOIN blog.UserGroup ug ON ug.ID = gm.GroupID
JOIN blog.grouppermissions gp ON gp.groupid = gm.groupid
JOIN blog.permission perm ON perm.id = gp.permissionid
WHERE u.id = $1
GROUP BY u.Username, u.Nickname, u.Img_location, ug.Name, gm.PrimaryGroup, u.ID;

-- name: search-with-id-and-pwd*
SELECT *
FROM blog.Users u
WHERE u.id = $1 AND u.password = $2;

-- name: patch-user*
UPDATE blog.Users
SET nickname = $1,
    username = $2,
    password = $3
WHERE id = $4;

-- name: patch-user-img*
UPDATE blog.Users
SET img_location = $1
WHERE id = $2;
