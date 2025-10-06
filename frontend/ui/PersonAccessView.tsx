import type { PersonWithLinksFragment } from '@/graphql/Person';
import { AddToPersonButton } from '@/ui/AddToPersonButton';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { DropdownMenuTrigger } from '@/ui/dropdown';
import { formatOpenDateRange, fullDateFormatter } from '@/ui/format';
import { CreateInvitationForm } from '@/ui/forms/CreateInvitationForm';
import { UserProxyMenu } from '@/ui/menus/UserProxyMenu';
import React from 'react';
import { useAuth } from './use-auth';
import Link from 'next/link';

export function PersonAccessView({ item }: { item: PersonWithLinksFragment }) {
  const auth = useAuth();

  return (
    <div className="prose prose-accent mb-2">
      <div className="flex justify-between items-baseline flex-wrap gap-4">
        <h3>Přístupové údaje</h3>
        <AddToPersonButton person={item} />
      </div>

      {item.userProxiesList?.map((proxy) => (
        <div className="flex gap-3 mb-3 items-start" key={proxy.id}>
          <UserProxyMenu align="start" data={proxy}>
            <DropdownMenuTrigger.RowDots />
          </UserProxyMenu>

          <div className="grow min-w-0 space-y-2 text-sm">
            <div className="flex flex-col sm:flex-row sm:items-baseline sm:justify-between gap-y-1 sm:gap-x-3">
              <b className="break-words">
                {proxy.user?.uEmail}
                {proxy.user?.uLogin ? `, ${proxy.user.uLogin}` : ''}
              </b>
              <span className="text-muted-foreground">{formatOpenDateRange(proxy)}</span>
            </div>

            <div className="flex flex-col gap-1 text-xs text-muted-foreground sm:text-sm">
              <div className="flex flex-col sm:flex-row sm:items-baseline sm:gap-2">
                <span className="font-medium text-foreground">Poslední aktivita</span>
                <span>
                  {proxy.user?.lastActiveAt
                    ? fullDateFormatter.format(new Date(proxy.user.lastActiveAt))
                    : '—'}
                </span>
              </div>
              <div className="flex flex-col sm:flex-row sm:items-baseline sm:gap-2">
                <span className="font-medium text-foreground">Verze aplikace</span>
                <span>{proxy.user?.lastVersion ?? '—'}</span>
              </div>
            </div>
          </div>
        </div>
      ))}

      <div className="flex justify-between items-baseline flex-wrap gap-4">
        <h3>Pozvánky</h3>
        <Dialog>
          <DialogTrigger.Add size="sm" />
          <DialogContent>
            <CreateInvitationForm person={item} />
          </DialogContent>
        </Dialog>
      </div>

      {item.personInvitationsList?.map((invitation) => (
        <div key={invitation.id}>
          {invitation.email}, vytvořena {fullDateFormatter.format(new Date(invitation.createdAt))}
          {auth.isAdmin && (
            <>
              {', '}
              <Link href={{ pathname: '/pozvanka', query: { token: invitation.accessToken } }}>
                link zde
              </Link>
            </>
          )}
        </div>
      ))}
    </div>
  );
}
