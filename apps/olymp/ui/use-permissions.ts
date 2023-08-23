import { tenantId } from '@app/tenant/config.js';

export class PermissionChecker {
  constructor(
    public userId: string,
    private attrs: {
      isTrainer: boolean;
      isAdministrator: boolean;
      coupleIds: string[];
      personIds: string[];
      tenantIds: string[];
    },
  ) {}

  get isTenantMember() {
    return this.attrs.tenantIds.includes(tenantId);
  }
  get isLoggedIn() {
    return !!this.userId;
  }
  get isTrainer() {
    return this.attrs.isTrainer;
  }
  get isAdmin() {
    return this.attrs.isAdministrator;
  }
  get isTrainerOrAdmin() {
    return this.attrs.isTrainer || this.attrs.isAdministrator;
  }

  isCurrentPerson(id: string | null) {
    return id && this.attrs.personIds.includes(id);
  }
  isCurrentCouple(id: string | null) {
    return id && this.attrs.coupleIds.includes(id);
  }
}
