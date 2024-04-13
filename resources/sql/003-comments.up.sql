CREATE TABLE blog.Comment
(
	ID SERIAL,
	parent_post_id INT NOT NULL,
	parent_comment_id INT NULL,
	Content TEXT NOT NULL DEFAULT '',
	creator_id INT NOT NULL,
	created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
	PRIMARY KEY(ID),
	FOREIGN KEY(creator_id) REFERENCES blog.users(ID)
		ON UPDATE CASCADE
		ON DELETE CASCADE,
	FOREIGN KEY(parent_post_id) REFERENCES blog.Post(ID)
		ON UPDATE CASCADE
		ON DELETE CASCADE,
	FOREIGN KEY(parent_comment_id) REFERENCES blog.Comment(ID)
		ON UPDATE CASCADE
		ON DELETE CASCADE
)
