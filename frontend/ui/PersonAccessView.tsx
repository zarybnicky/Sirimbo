import { DeleteInvitationDocument } from '@/graphql/Invitation';
import type { PersonWithLinksFragment } from '@/graphql/Person';
import { AddToPersonButton } from '@/ui/AddToPersonButton';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { formatOpenDateRange, fullDateFormatter } from '@/ui/format';
import { CreateInvitationForm } from '@/ui/forms/CreateInvitationForm';
import { UserProxyMenu } from '@/ui/menus/UserProxyMenu';
import { useAuth } from './use-auth';
import Link from 'next/link';
import * as React from 'react';
import { useMutation } from 'urql';

export function PersonAccessView({ item }: { item: PersonWithLinksFragment }) {
  const auth = useAuth();
  const deleteInvitation = useMutation(DeleteInvitationDocument)[1];

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
                {proxy.user ? (
                  <Link
                    href={{ pathname: '/users/[id]', query: { id: proxy.user.id } }}
                    className="underline"
                  >
                    {[proxy.user.uEmail, proxy.user.uLogin].filter(Boolean).join(', ')}
                  </Link>
                ) : (
                  '—'
                )}
              </b>
              <span className="text-neutral-10">{formatOpenDateRange(proxy)}</span>
            </div>
          </div>
        </div>
      ))}

      {auth.isAdmin && (
        <>
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
    </div>
  );
}
