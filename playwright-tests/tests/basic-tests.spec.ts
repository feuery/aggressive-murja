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

async function postPost(page, title, post, tag) {
    await page.getByTestId('new-post-btn').click();
    await page.getByTestId('clear-editor').click();

    console.log(`Posting post with title ${title}, tag ${tag}`);
    await expect(page.getByTestId('article-id')).toContainText('Article: No id');
    
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

    await page.locator('#editor-post-save').click();
    await page.getByTestId('home').click();
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
	await page.getByTestId('edit-post-btn').click();

	await expect(page.getByTestId('article-id')).not.toContainText('Article: No id');

	await expect(page.locator('#editor-post-content')).toBeVisible();

	await page.evaluate(() => {

	    document.querySelector('#editor-post-content').value = "edited article";
	    console.log('success');
	});

	await page.locator('#editor-post-save').click();
	await page.getByTestId('home').click();

	await expect(page.locator('.post')).toContainText("edited article");
	await expect(page.locator('.post')).not.toContainText(post);
    }

    await expect(page.locator('.meta')).toContainText('1, 2, 3, 4, 5, 6, 7, 8, 9, 10');

    // hide the post

    await page.getByTestId('edit-post-btn').click();
    
    await expect(page.locator('#editor-post-content')).toBeVisible();

    tag = 'hidden';
    await page.locator('#new-tag-btn').click();
    await page.locator('#tag-select').selectOption('hidden');
    await page.locator('#editor-post-save').click();
    await page.getByTestId('home').click();

    await expect(page.locator('.post')).toBeHidden();
    // make it visible

    await page.goto('http://localhost:3010/blog/postadmin');
    await expect(page.getByTestId('manager-edit-post-btn')).toBeVisible();
    
    await page.getByTestId('manager-edit-post-btn').click();
    await page.locator('#tag-select').selectOption('hidden');

    await page.getByTestId('remove-tag').click();
    await page.locator('#editor-post-title').fill('Latest test post');
    
    await page.locator('#editor-post-save').click();    
    await page.getByTestId('home').click();

    await page.getByTestId('edit-post-btn').click();

    await expect(page.locator('#editor-post-content')).toBeVisible();

    await page.evaluate(() => {
	document.querySelector('#editor-post-content').value = "jeejee";
	console.log('success');
    });	    
    await page.locator('#editor-post-save').click();
    await page.getByTestId('home').click();

    const new_ctx = await browser.newContext();
    // Create a new page inside context.
    const anon_page = await new_ctx.newPage();
    await anon_page.goto('http://localhost:3010');

    await expect(anon_page.locator('.meta')).toContainText('1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13'); 

    await anon_page.close();
    // make sure editor saves 
    await page.getByText('Edit this post').click();

    const edited_test_title = 'really badly edited test post';
    await page.locator('#editor-post-title').fill(edited_test_title);

    await page.locator('#editor-post-save').click();
    await page.getByTestId('home').click();

    await page.reload();

    console.log('Trying to post a new post');
    tag = 'test-tag'; 
    await postPost(page, 'A completely new post', 'random content', tag);
    await page.goto('http://localhost:3010');
    
    await expect(page.getByText('2024 (2)')).toBeVisible();
    await expect(page.getByText('random content')).toBeVisible();
    await expect(page.getByRole('link', { name: edited_test_title })).toBeVisible();

    // make sure the basic post view opens
    await page.getByRole('link', { name: 'really badly edited test post' }).click();
    await expect(page.getByText('LOADING')).toBeHidden();
    await expect(page.getByText('jeejee')).toBeVisible();
    await expect(page.getByText('random content')).toBeHidden();
});;

