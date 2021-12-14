const pg = require('pg');
const process = require('process');
const express = require('express');
const cookieParser = require('cookie-parser');

const { Pool } = require('pg');
const pool = new Pool();

const app = express();
app.use(cookieParser());

app.get('/graphql-auth', async function (req, res) {
  const phpsessid = req.cookies.PHPSESSID;
  if (!phpsessid) return res.json({ 'X-Hasura-Role': 'anonymous' });

  const sessRes = await pool.query(`SELECT * FROM session WHERE ss_id='${phpsessid}'`);
  if (!sessRes.rows[0]) return res.json({ 'X-Hasura-Role': 'anonymous' });

  const uid = JSON.parse(sessRes.rows[0].ss_data).id
  if (!uid) return res.json({ 'X-Hasura-Role': 'anonymous' });

  const userRes = await pool.query(`SELECT * FROM users WHERE u_id=${uid}`);
  if (!userRes.rows[0]) return res.json({ 'X-Hasura-Role': 'anonymous' });

  return res.json({
    'X-Hasura-Role': (
      userRes.rows[0].u_group == "0" ? "anonymous" :
      userRes.rows[0].u_group == "1" ? "admin" :
      "member"
    ),
    'X-Hasura-User-Id': uid.toString(),
  })
});

app.get('/logout', async function (req, res) {
  const phpsessid = req.cookies.PHPSESSID;
  if (phpsessid) {
    await pool.query(`DELETE FROM session WHERE ss_id='${phpsessid}'`);
  }
  return res.clearCookie('PHPSESSID', { path: '/' }).redirect('/');
});

app.listen(process.env.PORT || 4000)
