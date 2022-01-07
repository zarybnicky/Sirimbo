import { Pool } from 'pg';

export const pool = new Pool();

export const getSession = async (phpsessid: string | undefined) => {
  const sessRes = await pool.query(`SELECT * FROM session WHERE ss_id='${phpsessid}'`);
  return sessRes.rows[0];
};

export const getUser = async (uid: string | undefined) => {
  const userRes = await pool.query(`SELECT * FROM users WHERE u_id=${uid}`);
  return userRes.rows[0];
};
