import { PersonWithLinksFragment } from '@/graphql/Person';
import { AddToPersonButton } from '@/ui/AddToPersonButton';
import { UserProxyMenu } from '@/ui/UserProxyMenu';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { DropdownMenu, DropdownMenuTrigger } from '@/ui/dropdown';
import { formatOpenDateRange, fullDateFormatter } from '@/ui/format';
import { CreateInvitationForm } from '@/ui/forms/CreateInvitationForm';
import { useAuth } from '@/ui/use-auth';
import React from 'react';

export function PersonAccessView({ item }: { item: PersonWithLinksFragment }) {
  const auth = useAuth();

  return (
    <div className="prose prose-accent mb-2">
      <div className="flex justify-between items-baseline flex-wrap gap-4">
        <h3>Přístupové údaje</h3>
        <AddToPersonButton person={item} />
      </div>

      {item.userProxiesList?.map((item) => (
        <div className="flex gap-3 mb-1 align-baseline" key={item.id}>
          {auth.isAdmin && (
            <DropdownMenu>
              <DropdownMenuTrigger.RowDots />
              <UserProxyMenu data={item} />
            </DropdownMenu>
          )}
          <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
            <b>{item.user?.uEmail}, {item.user?.uLogin}</b>
            <span>{formatOpenDateRange(item)}</span>
          </div>
        </div>
      ))}

      <div className="flex justify-between items-baseline flex-wrap gap-4">
        <h3>Pozvánky</h3>
        <Dialog>
          <DialogTrigger.Add />
          <DialogContent>
            <CreateInvitationForm person={item} />
          </DialogContent>
        </Dialog>
      </div>

      {item.personInvitationsList?.map((item) => (
        <div key={item.id}>
          {item.email}, vytvořena {fullDateFormatter.format(new Date(item.createdAt))}
        </div>
      ))}
    </div>
  );
}
