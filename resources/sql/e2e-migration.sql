DELETE FROM blog.Users;
DELETE FROM blog.Media;

INSERT INTO blog.Users (username, nickname, img_location, password)
VALUES ('Playwright-user', 'playwrighte', '', 'fe1c94d7b0ac51013b944b84cd1fd40111421684f857ebf2eaaf494014007e3068ea25ed03c4c8155b33c1271742db38dcf3059096ad2b859b44ed2b9ae5d10f');
