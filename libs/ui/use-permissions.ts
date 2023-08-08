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
}

export const defaultPermissions: {
  [key in keyof typeof PermissionKey]: PermissionLevel;
} = {
  peAkce: 1,
  peAktuality: 2,
  peDokumenty: 1,
  peGalerie: 2,
  peNabidka: 1,
  peNastenka: 1,
  peNovinky: 1,
  pePary: 1,
  pePlatby: 1,
  pePermissions: 1,
  peRozpis: 1,
  peSkupiny: 2,
  peUsers: 1,
  peMain: 2,
};

export class PermissionChecker {
  public currentTenant = process.env.NEXT_PUBLIC_TENANT_ID ?? '1';
  constructor(
    public userId: string,
    public perms: { [key in keyof typeof PermissionKey]: number } = defaultPermissions,
    public attrs: {
      isTrainer: boolean;
      isAdministrator: boolean;
      coupleIds: string[];
      personIds: string[];
      tenantIds: string[];
    },
  ) {}

  get isTenantMember() { return this.attrs.tenantIds.includes(this.currentTenant) }
  isCurrentPerson(id: string | null) { return id && this.attrs.personIds.includes(id) }
  isCurrentCouple(id: string | null) { return id && this.attrs.coupleIds.includes(id) }
  get isLoggedIn() { return !!this.userId }
  get isTrainer() { return this.attrs.isTrainer }
  get isAdmin() { return this.attrs.isAdministrator }
  get isTrainerOrAdmin() { return this.attrs.isTrainer || this.attrs.isAdministrator }

  public hasPermission(key: PermissionKey, level: PermissionLevel) {
    const perm = PermissionKey[key] as keyof typeof PermissionKey;
    return this.perms[perm] >= level;
  }
}
