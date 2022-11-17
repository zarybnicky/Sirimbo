import { pool } from 'lib/PgPool';
import { GetServerSideProps, GetServerSidePropsContext, GetServerSidePropsResult } from 'next';
import { defaultPermissions, PermissionChecker, PermissionKey, PermissionLevel } from './use-permissions';

export { PermissionKey, PermissionLevel };

export async function useServerPermissions(context: GetServerSidePropsContext) {
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

export const withServerPermissions = (
  key: PermissionKey, level: PermissionLevel,
  callback?: (context: GetServerSidePropsContext) => Promise<object>
): GetServerSideProps => async (context) => {
  const perms = await useServerPermissions(context);
  if (!perms.hasPermission(key, level)) {
    if (!perms.userId) {
      return perms.redirectToLogin();
    } else {
      return perms.redirectToAuthError();
    }
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

  public redirectToLogin(): GetServerSidePropsResult<object> {
    const params = new URLSearchParams({
      from: this.context.resolvedUrl
    });
    return {
      redirect: {
        statusCode: 301,
        destination: `/login?${params}`
      }
    };
  }

  public redirectToAuthError(): GetServerSidePropsResult<object> {
    return {
      redirect: {
        statusCode: 301,
        destination: `/error?id=authorization`
      }
    };
  }
}
