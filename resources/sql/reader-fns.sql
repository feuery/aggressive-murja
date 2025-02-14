-- name: get-user-feeds*
-- returns: :array-hash 
SELECT fs.id, fs.name, fs.url,
       json_build_object( 'username',
       			  u.Username,
			  'nickname',
			  u.Nickname,
			  'img_location',
			  u.Img_location) as "creator",
       json_agg(row_to_json(fi.*)) as "items"
FROM blog.feed_subscription fs
JOIN blog.Users u ON u.ID = fs.owner
LEFT JOIN blog.feed_item fi ON (fs.id = fi.feed AND (read_at is null OR $2) AND fi.pubdate <= now())
WHERE owner = $1
GROUP BY fs.id, u.username, u.nickname, u.img_location
ORDER BY fs.name;

-- name: delete-feed
DELETE FROM blog.feed_subscription
WHERE id = $1 AND owner = $2;

-- name: get-all-feeds 
-- returns: :array-hash 
SELECT fs.id, fs.name, fs.url,
       json_build_object( 'username',
       			  u.Username,
			  'nickname',
			  u.Nickname,
			  'img_location',
			  u.Img_location) as "creator", fs.last_modified
FROM blog.feed_subscription fs
JOIN blog.Users u ON u.ID = fs.owner;

-- name: insert-feed @execute
INSERT INTO blog.feed_subscription(name, url, owner) VALUES ($1, $2, $3);

-- name: insert-feed-item @execute
INSERT INTO blog.feed_item(title, link, description, author, pubdate, feed)
VALUES ($1, $2, $3, $4, to_timestamp($5), $6) ON CONFLICT DO NOTHING;

-- name: mark-as-read
UPDATE blog.feed_item fi 
SET read_at = now()
FROM blog.feed_subscription fs
WHERE fs.id = fi.feed
      AND fi.id = $1
      AND fi.feed = $2
      AND fs.owner = $3
RETURNING *;

-- name: get-feed-name-and-url
-- returns: :array-hash
SELECT fs.name, fs.url
FROM blog.feed_subscription fs
JOIN blog.feed_item fi ON fi.feed = fs.id
WHERE fi.id = $1 AND fs.owner = $2;

-- name: update-last-modified @execute
UPDATE blog.feed_subscription
SET last_modified = $2
WHERE id = $1;
