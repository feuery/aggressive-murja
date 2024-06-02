-- name: get-user-feeds*
-- returns: :array-hash 
SELECT fs.id, fs.name, fs.url,
       json_build_object( 'username',
       			  u.Username,
			  'nickname',
			  u.Nickname,
			  'img_location',
			  u.Img_location) as "creator"
FROM blog.feed_subscription fs
JOIN blog.Users u ON u.ID = fs.owner
WHERE owner = $1;

-- name: get-all-feeds 
-- returns: :array-hash 
SELECT fs.id, fs.name, fs.url,
       json_build_object( 'username',
       			  u.Username,
			  'nickname',
			  u.Nickname,
			  'img_location',
			  u.Img_location) as "creator"
FROM blog.feed_subscription fs
JOIN blog.Users u ON u.ID = fs.owner;

-- name: insert-feed @execute
INSERT INTO blog.feed_subscription(name, url, owner) VALUES ($1, $2, $3);

-- name: insert-feed-item @execute
INSERT INTO blog.feed_item(title, link, description, author, pubdate, feed)
VALUES ($1, $2, $3, $4, to_timestamp($5), $6);
