import { pool } from 'lib/PgPool';
import { GetServerSideProps, GetServerSidePropsContext } from 'next';
import { defaultPermissions, PermissionChecker, PermissionKey, PermissionLevel } from './use-permissions';

export { PermissionKey, PermissionLevel };

export async function loadServerPermissions(context: GetServerSidePropsContext) {
  const { rows: [session] } = await pool.query(`
SELECT u_id, u_group, p_id, ss_id, permissions.* FROM session
LEFT JOIN users ON u_id=ss_user
LEFT JOIN permissions ON u_group=pe_id
LEFT JOIN public.pary ON (
  (p_id_partner = u_id and p_archiv = false) or
  (p_id_partnerka = u_id and p_archiv = false)
)
WHERE ss_id='${context.req.cookies.PHPSESSID}'
  `);
  return new ServerPermissionChecker(context, session?.u_id, session?.p_id, !session ? defaultPermissions : {
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
  });
}

export const withServerLoggedOut: GetServerSideProps = async (context) => {
  const perms = await loadServerPermissions(context);
  if (perms.userId) {
    return {
      redirect: {
        statusCode: 301,
        destination: '/dashboard',
      }
    };
  }
  return { props: {} };
};

export const withServerPermissions = (
  key: PermissionKey, level: PermissionLevel,
  callback?: (context: GetServerSidePropsContext) => Promise<object>
): GetServerSideProps => async (context) => {
  const perms = await loadServerPermissions(context);

  if (!perms.hasPermission(key, level)) {
    const params = new URLSearchParams({ from: context.resolvedUrl });
    return {
      redirect: {
        statusCode: 301,
        destination: !perms.userId ? `/login?${params}` : `/error?id=authorization`,
      }
    };
  }
  return { props: callback ? await callback(context) : {} };
};

class ServerPermissionChecker extends PermissionChecker {
  constructor(
    public context: GetServerSidePropsContext,
    userId: string,
    coupleId: string,
    perms: { [key in keyof typeof PermissionKey]: number }
  ) {
    super(userId, coupleId, perms);
  }
}
