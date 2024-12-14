CREATE TABLE blog.loggroup_reads
(
  group_id int not null,
  user_id int not null,
  read_count int not null default 0,
  PRIMARY KEY (group_id, user_id),
  FOREIGN KEY (group_id) references blog.log_group(ID)
  ON DELETE CASCADE
  ON UPDATE CASCADE,
  FOREIGN KEY (user_id) references blog.Users(ID)
  ON DELETE CASCADE
  ON UPDATE CASCADE
);
