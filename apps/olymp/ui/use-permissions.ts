export enum PermissionLevel {
  P_NONE = 1,
  P_VIEW = 2,
  P_MEMBER = 4,
  P_OWNED = 8,
  P_ADMIN = 16,
}

export class PermissionChecker {
  public currentTenant = process.env.NEXT_PUBLIC_TENANT_ID ?? '1';
  constructor(
    public userId: string,
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
}
