import { useAuth } from './use-auth';

export enum PermissionLevel {
  P_NONE = 1,
  P_VIEW = 2,
  P_MEMBER = 4,
  P_OWNED = 8,
  P_ADMIN = 16,
}

export enum PermissionKey {
  peAkce,
  peAktuality,
  peDokumenty,
  peGalerie,
  peNabidka,
  peNastenka,
  peNovinky,
  pePary,
  pePlatby,
  pePermissions,
  peRozpis,
  peSkupiny,
  peUsers,
  peMain,
};

type PermissionMap = { [key in keyof typeof PermissionKey]: PermissionLevel };

export const defaultPermissions: PermissionMap = {
  peAkce: 1,
  peAktuality: 1,
  peDokumenty: 1,
  peGalerie: 1,
  peNabidka: 1,
  peNastenka: 1,
  peNovinky: 1,
  pePary: 1,
  pePlatby: 1,
  pePermissions: 1,
  peRozpis: 1,
  peSkupiny: 1,
  peUsers: 1,
  peMain: 2,
};

export const usePermissions = (): PermissionMap => {
  const { user } = useAuth();

  return user?.permissionByUGroup || defaultPermissions;
};

export const useCheckPermission = (key: keyof typeof PermissionKey, level: PermissionLevel): boolean => {
  const perms = usePermissions();
  return perms[key] >= level;
}
