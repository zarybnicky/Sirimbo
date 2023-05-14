import { pool } from 'lib/PgPool';
import { GetServerSideProps, GetServerSidePropsContext } from 'next';
import {
  defaultPermissions,
  PermissionChecker,
  PermissionKey,
  PermissionLevel,
} from './use-permissions';

export { PermissionKey, PermissionLevel };

async function loadServerPermissions(req: GetServerSidePropsContext['req']) {
  const {
    rows: [session],
  } = await pool.query(`
SELECT u_id, permissions.* FROM session
LEFT JOIN users ON u_id=ss_user
LEFT JOIN permissions ON u_group=pe_id
WHERE ss_id='${req.cookies.PHPSESSID}'
  `);
  return new PermissionChecker(
    session?.u_id,
    !session
      ? defaultPermissions
      : {
          peAkce: session.pe_akce,
          peAktuality: session.pe_aktuality,
          peDokumenty: session.pe_dokumenty,
          peGalerie: session.pe_galerie,
          peMain: session.pe_main,
          peNabidka: session.pe_nabidka,
          peNastenka: session.pe_nastenka,
          peNovinky: session.pe_novinky,
          pePary: session.pe_pary,
          pePermissions: session.pe_permissions,
          pePlatby: session.pe_platby,
          peRozpis: session.pe_rozpis,
          peSkupiny: session.pe_skupiny,
          peUsers: session.pe_users,
        },
  );
}

export const withServerPermissions =
  (
    key: PermissionKey,
    level: PermissionLevel,
    callback?: (context: GetServerSidePropsContext) => Promise<object>,
  ): GetServerSideProps =>
  async (context) => {
    const perms = await loadServerPermissions(context.req);
    if (!perms.hasPermission(key, level)) {
      const params = new URLSearchParams({ from: context.resolvedUrl });
      return {
        redirect: {
          statusCode: 301,
          destination: !perms.userId ? `/login?${params}` : `/error?id=authorization`,
        },
      };
    }
    return { props: callback ? await callback(context) : {} };
  };
