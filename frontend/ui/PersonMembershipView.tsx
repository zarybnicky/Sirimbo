import { CoupleFragment } from '@/graphql/Memberships';
import type { PersonWithLinksFragment } from '@/graphql/Person';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import {
  formatLongCoupleName,
  formatOpenDateRange,
  fullDateFormatter,
  moneyFormatter,
} from '@/ui/format';
import { AddToCohortForm } from '@/ui/forms/AddToCohortForm';
import { CreateCoupleForm } from '@/ui/forms/CreateCoupleForm';
import { useAuth } from '@/ui/use-auth';
import { useAtomValue } from 'jotai';
import Link from 'next/link';
import React from 'react';
import { tenantIdAtom } from '@/ui/state/auth';
import { AddToPersonButton } from '@/ui/AddToPersonButton';
import { CreateInvitationForm } from '@/ui/forms/CreateInvitationForm';
import { keyIsNonNull } from '@/lib/truthyFilter';
import { useActionMap, useActions } from '@/lib/actions';
import { cohortMembershipActions } from '@/lib/actions/cohortMembership';
import { coupleActions } from '@/lib/actions/couple';
import { personInvitationActions } from '@/lib/actions/personInvitation';
import { tenantAdministratorActions } from '@/lib/actions/tenantAdministrator';
import { tenantMembershipActions } from '@/lib/actions/tenantMembership';
import { tenantTrainerActions } from '@/lib/actions/tenantTrainer';
import { userProxyActions } from '@/lib/actions/userProxy';
import { ActionRow } from '@/ui/ActionRow';
import { slugify } from '@/lib/slugify';

export function PersonMembershipView({ item }: { item: PersonWithLinksFragment }) {
  const auth = useAuth();
  const isAdminOrCurrentPerson = auth.isAdmin || auth.isMyPerson(item.id);
  const tenantId = useAtomValue(tenantIdAtom);
  const cohortMembershipActionMap = useActionMap(
    cohortMembershipActions,
    item.cohortMembershipsList,
  );
  const tenantMembershipActionMap = useActionMap(
    tenantMembershipActions,
    item.tenantMembershipsList,
  );
  const tenantAdministratorActionMap = useActionMap(
    tenantAdministratorActions,
    item.tenantAdministratorsList,
  );
  const tenantTrainerActionMap = useActionMap(
    tenantTrainerActions,
    item.tenantTrainersList,
  );
  const userProxyActionMap = useActionMap(userProxyActions, item.userProxiesList ?? []);
  const invitationActionMap = useActionMap(
    personInvitationActions,
    item.personInvitationsList ?? [],
  );

  return (
    <div key="info" className="mb-2">
      <div className="flex justify-between items-baseline flex-wrap gap-4">
        <h3 className="text-lg font-semibold mt-4 mb-2">Páry</h3>

        {auth.isAdmin && (
          <Dialog modal={false}>
            <DialogTrigger.Add size="sm" />
            <DialogContent>
              <CreateCoupleForm person={item} />
            </DialogContent>
          </Dialog>
        )}
      </div>

      {item.allCouplesList
        ?.toSorted((a, b) => a.since.localeCompare(b.since))
        ?.map((item) => (
          <CoupleRow key={item.id} item={item} />
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
          <ActionRow key={item.id} actions={cohortMembershipActionMap.get(item.id)!}>
            <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
              <Link
                className="underline font-bold"
                href={{
                  pathname: '/treninkove-skupiny/[id]/[...slug]',
                  query: { id: item.cohort.id, slug: [slugify(item.cohort.name)] },
                }}
              >
                {item.cohort.name}
              </Link>
              <span>{formatOpenDateRange(item)}</span>
            </div>
          </ActionRow>
        ))}

      <div className="flex justify-between items-baseline flex-wrap gap-4">
        <h3 className="text-lg font-semibold mt-4 mb-2">Členství</h3>
      </div>

      {item.tenantAdministratorsList
        .filter((x) => x.tenantId === tenantId)
        .map((item) => (
          <ActionRow key={item.id} actions={tenantAdministratorActionMap.get(item.id)!}>
            <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
              <b>Správce</b>
            </div>
            {auth.isAdmin && <span>{formatOpenDateRange(item)}</span>}
          </ActionRow>
        ))}

      {item.tenantTrainersList
        .filter((x) => x.tenantId === tenantId && x.status === 'ACTIVE')
        .map((item) => (
          <ActionRow key={item.id} actions={tenantTrainerActionMap.get(item.id)!}>
            <div className="grow gap-4 align-baseline flex flex-wrap justify-between text-sm py-1">
              <b>Trenér</b>
              {auth.isAdmin && (
                <>
                  <div>
                    {moneyFormatter.format({
                      amount: item.memberPrice45MinAmount,
                      currency: item.currency,
                    }) || '-'}{' '}
                    {item.guestPrice45MinAmount &&
                    item.memberPrice45MinAmount !== item.guestPrice45MinAmount
                      ? `(${moneyFormatter.format({ amount: item.guestPrice45MinAmount, currency: item.currency })})`
                      : ''}
                    {' / 45min'}
                  </div>
                  <span>{formatOpenDateRange(item)}</span>
                </>
              )}
            </div>
          </ActionRow>
        ))}
      {item.tenantMembershipsList
        .filter((x) => x.tenantId === tenantId)
        .map((item) => (
          <ActionRow key={item.id} actions={tenantMembershipActionMap.get(item.id)!}>
            <div className="grow gap-4 align-baseline flex flex-wrap justify-between text-sm py-1">
              <b>Člen</b>
              {auth.isAdmin && <span>{formatOpenDateRange(item)}</span>}
            </div>
          </ActionRow>
        ))}

      {isAdminOrCurrentPerson && (
        <>
          <div className="flex justify-between items-baseline flex-wrap gap-4">
            <h3 className="text-lg font-semibold mt-4 mb-2">Přístupové údaje</h3>
            <AddToPersonButton person={item} />
          </div>

          {item.userProxiesList?.filter(keyIsNonNull('user')).map((proxy) => (
            <ActionRow key={proxy.id} actions={userProxyActionMap.get(proxy.id)!}>
              <div className="grow flex flex-col sm:flex-row sm:items-baseline sm:justify-between text-sm gap-y-1 sm:gap-x-3">
                <b className="break-words">
                  <Link
                    href={{ pathname: '/users/[id]', query: { id: proxy.user.id } }}
                    className="underline"
                  >
                    {[proxy.user.uEmail, proxy.user.uLogin].filter(Boolean).join(', ')}
                  </Link>
                </b>
                <span>{formatOpenDateRange(proxy)}</span>
              </div>
            </ActionRow>
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
                <ActionRow
                  key={invitation.id}
                  actions={invitationActionMap.get(invitation.id)!}
                >
                  <div className="grow flex flex-col sm:flex-row sm:items-baseline sm:justify-between gap-y-1 sm:gap-x-3 text-sm py-1">
                    <b className="break-words">{invitation.email}</b>
                    <span>
                      vytvořena {fullDateFormatter.format(new Date(invitation.createdAt))}
                    </span>
                  </div>
                </ActionRow>
              ))}
            </>
          )}
        </>
      )}
    </div>
  );
}

function CoupleRow({ item }: { item: CoupleFragment }) {
  const actions = useActions(coupleActions, item);
  return (
    <ActionRow actions={actions}>
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
    </ActionRow>
  );
}
