CREATE TABLE IF NOT EXISTS blog.Previously_Link
(
	referencee_id INT NOT NULL,
	referenced_id INT NOT NULL,
	PRIMARY KEY (referencee_id, referenced_id),
	FOREIGN KEY (referencee_id) REFERENCES blog.Post(id) ON UPDATE CASCADE ON DELETE CASCADE,
	FOREIGN KEY (referenced_id) REFERENCES blog.Post(id) ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE OR REPLACE VIEW blog.Previously_Link_Titles AS
        SELECT pl.referencee_id,
               pl.referenced_id AS id,
	       p.Title AS title
	FROM blog.Previously_Link pl
	JOIN blog.Post p on pl.referenced_id = p.id;

INSERT INTO blog.Settings VALUES ('previously_label', '"Previously"') ON CONFLICT DO NOTHING;
