// @ts-check
import { test, expect } from '@playwright/test';

// we're expecting there to be a actived non-banned user with username-displayname-password triple of ('playwright', 'Test User', 'p4ssw0rd') set up by the calling test fixture. This isn't possible to set up in the

const username = 'Playwright-user';
const nickname = 'playwrighte';
const password = 'p4ssw0rd';

// async function login(page) {
//   await page.locator('#username').fill(username);
//   await page.locator('#password').fill(password);

//   await page.locator('#login_btn').click();
  
//   await expect(page.getByText('Log out')).toBeVisible();
// }

async function postPost(page, title, post, tag, append_img = false) {
    await page.getByTestId('new-post-btn').click();

    console.log(`Posting post with title ${title}, tag ${tag}`);
    
    await page.locator('#editor-post-title').fill(title);
    
    await page.evaluate((post) => {
	document.querySelector('#editor-post-content').value = post;
    }, post);

    await expect(page.locator('#editor-post-title')).toHaveValue(title);
    await expect(page.locator('#editor-post-content')).toContainText(post);

    // edit tags

    await expect(page.locator('#tag-select')).not.toContainText(tag);

    await page.locator('#new-tag-btn').click();

    await page.locator('#tag-select').selectOption(tag);

    await expect(page.locator('#tag-select')).toContainText(tag);

    // make sure preview works
    await expect(page.locator('#show-preview-cb')).not.toBeChecked();
    await expect(page.locator('.post')).toBeHidden();

    await page.locator('#show-preview-cb').click();

    await expect(page.locator('.post')).toBeVisible();
    await page.locator('#show-preview-cb').click();

    await expect(page.locator('.post')).toBeHidden();

    if (append_img) {

	await page.evaluate(() => {
	    document.querySelector('#file-pictures-input').style["display"] = 'inline';
	});

	const filePath = __dirname+'/test.png';
	console.log("Inputting file " + filePath);
	
	await page.locator('#file-pictures-input').setInputFiles(filePath);

	await page.evaluate(() => {
	    document.querySelector('#file-pictures-input').style["display"] = 'none';
	});
	await expect(page.locator('#editor-post-content')).toContainText('<img');
    }

    
    await page.locator("#hidden").setChecked(false);

    // save the post

    await page.locator('#editor-post-save').click();
    await page.goto('http://localhost:3010');
}

test('basic testing', async ({ page, browser }) => {
    await page.goto('http://localhost:3010');

    await expect(page.locator('.post')).toBeHidden();

    // these shoudln't probably be visible in a completely empty murja but :shrug:
    /*
    await expect(page.getByTestId('page-changer')).toHaveText('Next page');
    await expect(page.getByTestId('page-changer')).not.toHaveText('Previous page'); */

    // login-form is visible, logged-in component is hidden
    await expect(page.getByTestId('username-input-field')).toBeVisible();
    await expect(page.getByTestId('welcome-user-label')).toBeHidden();

    await page.getByTestId('username-input-field').fill(username);
    await page.getByTestId('password-input-field').fill(password);

    await page.getByTestId('dologin').click();
    await expect(page.getByTestId('loginfailed')).toBeHidden();
    await expect(page.getByTestId('welcome-user-label')).toBeVisible();
    await expect(page.getByTestId('welcome-user-label')).toHaveText(`Welcome, ${nickname}`);

    // test post editor
    await expect(page.locator('.post')).toBeHidden()
    const title = 'New automatic post';
    let tag = 'test-tag';
    let first_dialog_event = dialog => {
	console.log(`accepting tag ${tag}`);
	dialog.accept(tag);
    };

    const post= 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer enim erat, sollicitudin non nibh et, porttitor ullamcorper nunc. Phasellus eget orci molestie leo euismod blandit. Nam sagittis porta lectus in pulvinar. Suspendisse ultricies, eros ac venenatis vestibulum, est elit varius sem, quis vehicula quam nibh ut lorem. Nullam eget lobortis magna, in tristique diam. Interdum et malesuada fames ac ante ipsum primis in faucibus. Maecenas ultrices sapien dolor, ut suscipit mauris luctus et. Nullam tristique lorem in volutpat venenatis. Nunc ultrices nulla libero, et dignissim metus eleifend et. Donec congue mauris felis, pulvinar vulputate enim efficitur in. Pellentesque vehicula maximus lorem, eu tempus tortor iaculis eu. Aliquam lectus turpis, aliquam quis mattis nec, placerat nec erat. Morbi et justo et neque elementum suscipit. Nullam rutrum lectus eget lacus viverra pellentesque. Fusce faucibus lacinia urna at tincidunt. In hac habitasse platea dictumst.';
    
    page.on('dialog', first_dialog_event);

    await postPost(page, title, post, tag);

    // are there any posts?
    await page.goto('http://localhost:3010');
    
    await expect(page.locator('.post')).toBeVisible();
    await expect(page.locator('.post')).toContainText(post);
    await expect(page.locator('.tag')).toHaveText(tag);

    // edit the post
    for(let x = 0; x < 10; x++) {
	const hidden = x % 2 == 0;
	await page.goto('http://localhost:3010/blog/postadmin');
	await expect(page.getByTestId('manager-edit-post-btn')).toBeVisible();
    
	await page.getByTestId('manager-edit-post-btn').click();

	await expect(page.locator('#editor-post-content')).toBeVisible();

	await page.evaluate(() => {

	    document.querySelector('#editor-post-content').value = "edited article";
	    console.log('success');
	});

	await page.locator("#hidden").setChecked(hidden);

	await page.locator('#editor-post-save').click();
	await page.goto('http://localhost:3010');

	if (hidden) {
	    await expect(page.locator('.post')).toBeHidden()
	} else {
	    await expect(page.locator('.post')).toContainText("edited article");
	    await expect(page.locator('.post')).not.toContainText(post);
	}
    }

    await expect(page.locator('.meta')).toContainText('2, 4, 6, 8, 10');

    // hide the post

    await page.getByTestId('edit-post-btn').click();
    
    await expect(page.locator('#editor-post-content')).toBeVisible();
    await page.locator("#hidden").setChecked(false);
    await page.locator('#editor-post-save').click();
    await page.goto('http://localhost:3010');

    await expect(page.locator('.post')).toBeHidden();
    // make it visible

    await page.goto('http://localhost:3010/blog/postadmin');
    await expect(page.getByTestId('manager-edit-post-btn')).toBeVisible();
    
    await page.getByTestId('manager-edit-post-btn').click();
    await page.locator("#hidden").setChecked(false);
    await page.locator('#editor-post-title').fill('Latest test post');
    
    await page.locator('#editor-post-save').click();
    await page.goto('http://localhost:3010');

    await page.getByTestId('edit-post-btn').click();

    await expect(page.locator('#editor-post-content')).toBeVisible();

    await page.evaluate(() => {
	document.querySelector('#editor-post-content').value = "jeejee";
	console.log('success');
    });	    
    await page.locator('#editor-post-save').click();
    await page.goto('http://localhost:3010');

    const new_ctx = await browser.newContext();
    // Create a new page inside context.
    const anon_page = await new_ctx.newPage();
    await anon_page.goto('http://localhost:3010');

    await expect(anon_page.locator('.meta')).toContainText('2, 4, 6, 8, 10, 12, 13, 14');
    
    await anon_page.close(); 
    // make sure editor saves 
    await page.getByText('Edit this post').click();

    const edited_test_title = 'really badly edited test post';
    await page.locator('#editor-post-title').fill(edited_test_title);

    await page.locator('#editor-post-save').click();
    await page.goto('http://localhost:3010');

    await page.reload();

    console.log('Trying to post a new post');
    tag = 'newer-test-tag'; 
    await postPost(page, 'A completely new post', 'random content', tag);
    await page.goto('http://localhost:3010');
    
    await expect(page.getByText('2024 (2)')).toBeVisible();
    await expect(page.getByText('random content')).toBeVisible();
    await expect(page.getByRole('link', { name: edited_test_title })).toBeVisible();

    // make sure the basic post view opens
    await page.getByRole('link', { name: 'really badly edited test post' }).click();
    // await expect(page.getByText('LOADING')).toBeHidden();
    await expect(page.getByText('jeejee')).toBeVisible(); 
    await expect(page.getByText('random content')).toBeHidden();

    // test images
    await postPost(page, 'Image post', 'this is image content', tag, true);
    await page.goto('http://localhost:3010');
    await postPost(page, 'Image post2', 'this is image content2', tag, true);

    // test media manager
    await page.getByText('Manage media').click();
    await expect(page.locator('details > img')).toHaveCount(2);

    // Test the referencing posts thing
    let referencing_details = await page.getByTestId("referencing-post").all();

    console.log('posts: ' + (await page.getByText('Referencing posts').all())); 
    
    await expect(referencing_details.length == 2).toBeTruthy();

    await referencing_details[0].click();
    await expect(page.getByRole('link', { name: 'Image post' })).toBeVisible();

    await referencing_details[1].click();
    await expect(page.getByRole('link', { name: 'Image post2' })).toBeVisible();
    
    // Test deletion

    for(let cb of await page.locator('input[type="checkbox"]').all()) {	
	await expect(cb).not.toBeChecked();
    }
    await page.getByText('Select all').click();

    for(let cb of await page.locator('input[type="checkbox"]').all()) {	
	await expect(cb).toBeChecked();
    }

    await page.evaluate(() => document.querySelectorAll('input[type="checkbox"]').forEach(input => input.checked = false));

    let checkbox_labels = await page.getByText('Choose for deletion').all();
    await checkbox_labels[0].click();
    await page.getByText('Remove selected').click();
    await expect(page.locator('details > img')).toHaveCount(1);

    await page.getByText('Select all').click();
    await page.getByText('Remove selected').click();
    await expect(page.locator('details > img')).toHaveCount(0);

    // Test tag based fetching
    await page.goto('http://localhost:3010/blog/tags/newer-test-tag');
    await expect(page.locator('.post')).toHaveCount(3);

    await page.goto('http://localhost:3010/blog/tags/test-tag');
    await expect(page.locator('.post')).toHaveCount(1);
    
});
