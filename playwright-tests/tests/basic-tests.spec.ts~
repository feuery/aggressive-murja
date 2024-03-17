// @ts-check
import { test, expect } from '@playwright/test';

// we're expecting there to be a actived non-banned user with username-displayname-password triple of ('playwright', 'Test User', 'p4ssw0rd') set up by the calling test fixture. This isn't possible to set up in the

const username = 'playwright';
const password = 'p4ssw0rd';

async function login(page) {
  await page.locator('#username').fill(username);
  await page.locator('#password').fill(password);

  await page.locator('#login_btn').click();
  
  await expect(page.getByText('Log out')).toBeVisible();
}

test('basic testing', async ({ page }) => {
  await page.goto('http://localhost:3000');
  const request = page.request.get('http://localhost:3000/api/session');
  
  await expect(page.getByText('Welcome to pichunter')).toBeVisible();

  await expect((await request).status()).toBe(401);

  await login(page);


  await expect((await page.request.get('http://localhost:3000/api/session')).status()).toBe(200);

  // await expect(page.locator('.user-meta .displayname')).toHaveText('Test User');

  await page.locator('#user_details_btn').click();
  await page.locator('#display_name_lbl').click();
  await expect(page.locator('#display_name')).toBeFocused();

  const edited_displayname = 'Edited Displayname';
  await page.locator('#display_name').fill(edited_displayname);
  await page.locator('#save_user_btn').click();

  await expect(page.locator('.user-meta .displayname')).toHaveText(edited_displayname);

  await page.locator('#display_name').fill('Test User');
  await page.locator('#save_user_btn').click();

  // the <a href="/" /> is broken in playwright for reasons 🤷
  await page.goto('http://localhost:3000');
  await expect((await page.request.get('http://localhost:3000/api/session')).status()).toBe(200);
  
  await page.getByText('Place pictures on the map').click();
  await page.locator('#county_chooser').selectOption({value: '6'});

  await expect(page.getByText('No image loaded yet')).toBeHidden();
  
});

