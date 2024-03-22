-- name: get-user-by-id*
-- returns: :array-hash

SELECT
  u.id,
  u.username,
  u.nickname,
  u.img_location,
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
-- :? :1 
SELECT u.Username, u.Nickname, u.ID as UserID, u.Password, u.Img_location, ug.ID as GroupID, ug.Name as GroupName, gm.PrimaryGroup, json_agg(DISTINCT perm.action) "permissions"
FROM blog.Users u
JOIN blog.GroupMapping gm ON u.ID = gm.UserID
JOIN blog.UserGroup ug ON ug.ID = gm.GroupID
JOIN blog.grouppermissions gp ON gp.groupid = gm.groupid
JOIN blog.permission perm ON perm.id = gp.permissionid
WHERE u.Username = $1 AND u.Password = $2;

-- name: get-user-view-data*
SELECT u.Username, u.Nickname, u.Img_location, ug.Name as "primary-group-name", gm.PrimaryGroup, u.ID as userid, perm.action
FROM blog.Users u
JOIN blog.GroupMapping gm ON u.ID = gm.UserID
JOIN blog.UserGroup ug ON ug.ID = gm.GroupID
JOIN blog.grouppermissions gp ON gp.groupid = gm.groupid
JOIN blog.permission perm ON perm.id = gp.permissionid
WHERE u.ID = :user-id;

-- name: user-groups*
SELECT ug.ID, ug.Name, ug.Description
FROM blog.Users u
LEFT JOIN blog.GroupMapping um ON um.UserID = u.ID
LEFT JOIN blog.UserGroup ug ON um.GroupID = ug.ID
WHERE u.Username = :username;


-- name: can?*
-- :? :1
SELECT COUNT(perm.ACTION) > 0 AS "can?"
FROM blog.GroupPermissions gp 
LEFT JOIN blog.Permission perm ON gp.PermissionID = perm.ID
WHERE gp.GroupID = :group-id AND perm.action = :action;
