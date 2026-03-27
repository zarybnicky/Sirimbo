import {
  CreateTenantAdministratorDocument,
  CreateTenantMembershipDocument,
  CreateTenantTrainerDocument,
} from '@/graphql/Memberships';
import type { PersonWithLinksFragment } from '@/graphql/Person';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import {
  formatLongCoupleName,
  fullDateFormatter,
  formatOpenDateRange,
  moneyFormatter,
} from '@/ui/format';
import { AddToCohortForm } from '@/ui/forms/AddToCohortForm';
import { CreateCoupleForm } from '@/ui/forms/CreateCoupleForm';
import { CohortMembershipMenu } from '@/ui/menus/CohortMembershipMenu';
import { CoupleMenu } from '@/ui/menus/CoupleMenu';
import { TenantAdministratorMenu } from '@/ui/menus/TenantAdministratorMenu';
import { TenantMembershipMenu } from '@/ui/menus/TenantMembershipMenu';
import { TenantTrainerMenu } from '@/ui/menus/TenantTrainerMenu';
import { buttonCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { useAtomValue } from 'jotai';
import { Plus } from 'lucide-react';
import Link from 'next/link';
import React from 'react';
import { useMutation } from 'urql';
import { tenantIdAtom } from '@/ui/state/auth';
import { DeleteInvitationDocument } from '@/graphql/Invitation';
import { AddToPersonButton } from '@/ui/AddToPersonButton';
import { CreateInvitationForm } from '@/ui/forms/CreateInvitationForm';
import { UserProxyMenu } from '@/ui/menus/UserProxyMenu';
import { keyIsNonNull } from './truthyFilter';

export function PersonMembershipView({ item }: { item: PersonWithLinksFragment }) {
  const auth = useAuth();
  const isAdminOrCurrentPerson = auth.isAdmin || auth.personIds.includes(item.id);
  const tenantId = useAtomValue(tenantIdAtom);
  const createTenantMember = useMutation(CreateTenantMembershipDocument)[1];
  const createTenantTrainer = useMutation(CreateTenantTrainerDocument)[1];
  const createTenantAdmin = useMutation(CreateTenantAdministratorDocument)[1];
  const deleteInvitation = useMutation(DeleteInvitationDocument)[1];

  const addAsMember = React.useCallback(
    () => createTenantMember({ input: { tenantMembership: { personId: item.id } } }),
    [createTenantMember, item.id],
  );
  const addAsTrainer = React.useCallback(
    () => createTenantTrainer({ input: { tenantTrainer: { personId: item.id } } }),
    [createTenantTrainer, item.id],
  );
  const addAsAdmin = React.useCallback(
    () => createTenantAdmin({ input: { tenantAdministrator: { personId: item.id } } }),
    [createTenantAdmin, item.id],
  );

  return (
    <div key="info" className="mb-2">
      <div className="flex justify-between items-baseline flex-wrap gap-4">
        <h3 className="text-lg font-semibold mt-4 mb-2">Páry</h3>

        {auth.isAdmin && (
          <Dialog modal={false}>
            <DialogTrigger.Add size="sm" />
            <DialogContent>
              <CreateCoupleForm initial={item} />
            </DialogContent>
          </Dialog>
        )}
      </div>

      {item.allCouplesList
        ?.toSorted((a, b) => a.since.localeCompare(b.since))
        ?.map((item) => (
          <div className="flex gap-3 mb-1 align-baseline" key={item.id}>
            <CoupleMenu align="start" data={item}>
              <DropdownMenuTrigger.RowDots />
            </CoupleMenu>

            <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
              <Link
                className="underline font-bold"
                href={{
                  pathname: '/pary/[id]',
                  query: { id: item.id },
                }}
              >
                {formatLongCoupleName(item)}
              </Link>
              <span>{formatOpenDateRange(item)}</span>
            </div>
          </div>
        ))}

      <div className="flex justify-between items-baseline flex-wrap gap-4">
        <h3 className="text-lg font-semibold mt-4 mb-2">Tréninkové skupiny</h3>

        {auth.isAdmin && (
          <Dialog modal={false}>
            <DialogTrigger.Add size="sm" />
            <DialogContent>
              <AddToCohortForm person={item} />
            </DialogContent>
          </Dialog>
        )}
      </div>
      {item.cohortMembershipsList
        .filter(keyIsNonNull('cohort'))
        .toSorted((x, y) => (x.person?.name || '').localeCompare(y.person?.name || ''))
        .map((item) => (
          <div className="flex gap-3 mb-1 align-baseline" key={item.id}>
            <CohortMembershipMenu align="start" data={item}>
              <DropdownMenuTrigger.RowDots />
            </CohortMembershipMenu>

            <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
              <Link
                className="underline font-bold"
                href={{
                  pathname: '/treninkove-skupiny/[id]',
                  query: { id: item.cohort.id },
                }}
              >
                {item.cohort.name}
              </Link>
              <span>{formatOpenDateRange(item)}</span>
            </div>
          </div>
        ))}

      <div className="flex justify-between items-baseline flex-wrap gap-4">
        <h3 className="text-lg font-semibold mt-4 mb-2">Členství</h3>

        {auth.isAdmin && (
          <DropdownMenu>
            <DropdownMenuTrigger
              className={buttonCls({ variant: 'outline', size: 'sm' })}
            >
              <Plus />
              Přidat
            </DropdownMenuTrigger>
            <DropdownMenuContent align="end">
              <DropdownMenuButton onClick={addAsAdmin}>jako správce</DropdownMenuButton>
              <DropdownMenuButton onClick={addAsTrainer}>jako trenéra</DropdownMenuButton>
              <DropdownMenuButton onClick={addAsMember}>jako člena</DropdownMenuButton>
            </DropdownMenuContent>
          </DropdownMenu>
        )}
      </div>

      {item.tenantAdministratorsList
        .filter((x) => x.tenantId === tenantId)
        .map((item) => (
          <div className="flex gap-3 mb-1" key={item.id}>
            <TenantAdministratorMenu align="start" data={item}>
              <DropdownMenuTrigger.RowDots />
            </TenantAdministratorMenu>

            <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
              <b>Správce klubu {item.tenantName}</b>
            </div>
            {auth.isAdmin && <span>{formatOpenDateRange(item)}</span>}
          </div>
        ))}

      {item.tenantTrainersList
        .filter((x) => x.tenantId === tenantId && x.status === 'ACTIVE')
        .map((item) => (
          <div className="flex gap-3 mb-1 align-baseline" key={item.id}>
            <TenantTrainerMenu align="start" data={item}>
              <DropdownMenuTrigger.RowDots />
            </TenantTrainerMenu>

            <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
              <b>Trenér v klubu {item.tenantName}</b>
              {auth.isAdmin && (
                <div>
                  {moneyFormatter.format({
                    amount: item.memberPrice45MinAmount,
                    currency: item.currency,
                  }) || '-'}{' '}
                  {item.guestPrice45MinAmount
                    ? `(${moneyFormatter.format({ amount: item.guestPrice45MinAmount, currency: item.currency })})`
                    : ''}
                  {' / 45min'}
                </div>
              )}
            </div>
          </div>
        ))}
      {item.tenantMembershipsList
        .filter((x) => x.tenantId === tenantId)
        .map((item) => (
          <div className="flex gap-3 mb-1 align-baseline" key={item.id}>
            <TenantMembershipMenu align="start" data={item}>
              <DropdownMenuTrigger.RowDots />
            </TenantMembershipMenu>

            <div className="grow align-baseline text-sm font-bold py-1">
              Člen klubu {item.tenantName}
            </div>
            {auth.isAdmin && <span>{formatOpenDateRange(item)}</span>}
          </div>
        ))}

      {isAdminOrCurrentPerson && (
        <>
          <div className="flex justify-between items-baseline flex-wrap gap-4">
            <h3 className="text-lg font-semibold mt-4 mb-2">Přístupové údaje</h3>
            <AddToPersonButton person={item} />
          </div>

          {item.userProxiesList?.filter(keyIsNonNull('user')).map((proxy) => (
            <div className="flex gap-3 mb-3 items-start" key={proxy.id}>
              <UserProxyMenu align="start" data={proxy}>
                <DropdownMenuTrigger.RowDots />
              </UserProxyMenu>

              <div className="grow min-w-0 space-y-2 text-sm">
                <div className="flex flex-col sm:flex-row sm:items-baseline sm:justify-between gap-y-1 sm:gap-x-3">
                  <b className="break-words">
                    <Link
                      href={{ pathname: '/users/[id]', query: { id: proxy.user.id } }}
                      className="underline"
                    >
                      {[proxy.user.uEmail, proxy.user.uLogin]
                        .filter(Boolean)
                        .join(', ')}
                    </Link>
                  </b>
                  <span className="text-neutral-10">{formatOpenDateRange(proxy)}</span>
                </div>
              </div>
            </div>
          ))}

          {auth.isAdmin && (
            <>
              <div className="flex justify-between items-baseline flex-wrap gap-4">
                <h3 className="text-lg font-semibold mt-4 mb-2">Pozvánky</h3>
                <Dialog>
                  <DialogTrigger.Add size="sm" />
                  <DialogContent>
                    <CreateInvitationForm person={item} />
                  </DialogContent>
                </Dialog>
              </div>

              {item.personInvitationsList?.map((invitation) => (
                <DropdownMenu key={invitation.id}>
                  <div className="flex gap-2">
                    <DropdownMenuTrigger.RowDots />
                    {invitation.email}, vytvořena{' '}
                    {fullDateFormatter.format(new Date(invitation.createdAt))}
                  </div>
                  <DropdownMenuContent>
                    <DropdownMenuButton
                      onClick={() =>
                        navigator.clipboard.writeText(
                          `${window.location.origin}/pozvanka?token=${invitation.accessToken}`,
                        )
                      }
                    >
                      Kopírovat odkaz
                    </DropdownMenuButton>
                    <DropdownMenuButton
                      onClick={() => deleteInvitation({ input: { id: invitation.id } })}
                    >
                      Zrušit pozvánku
                    </DropdownMenuButton>
                  </DropdownMenuContent>
                </DropdownMenu>
              ))}
            </>
          )}
        </>
      )}
    </div>
  );
}
