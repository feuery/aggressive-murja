CREATE TABLE IF NOT EXISTS blog.feed_subscription
(
	ID UUID PRIMARY KEY DEFAULT gen_random_uuid(),
	name TEXT NOT NULL,
	url TEXT NOT NULL,
	owner INT NOT NULL,
	FOREIGN KEY (owner) REFERENCES blog.Users(id)
	on delete cascade
	on update cascade
);

CREATE TABLE IF NOT EXISTS blog.feed_item
(
	ID UUID PRIMARY KEY DEFAULT gen_random_uuid(),
	fetched TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
	title TEXT NOT NULL,
	description TEXT NOT NULL,
	link TEXT NOT NULL,
	feed UUID NOT NULL,
	author TEXT NOT NULL,
	pubdate TIMESTAMP NOT NULL,
	FOREIGN KEY (feed) REFERENCES blog.feed_subscription (ID)
	on delete cascade
	on update cascade,
	UNIQUE (link, feed)
);
	
