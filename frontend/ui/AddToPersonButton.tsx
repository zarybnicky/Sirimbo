import { CreateUserProxyDocument } from "@/graphql/Memberships";
import React from "react";
import { useMutation, useQuery } from "urql";
import { UserListDocument } from "@/graphql/CurrentUser";
import { ComboboxSearchArea } from "@/ui/fields/Combobox";
import { Popover, PopoverTrigger } from '@/ui/popover';
import * as PopoverPrimitive from '@radix-ui/react-popover';
import { Plus } from 'lucide-react';
import { buttonCls } from "@/ui/style";
import { useAuth } from "@/ui/use-auth";

export function AddToPersonButton({ person }: { person: { id: string; } }) {
  const auth = useAuth();
  const [open, setOpen] = React.useState(false);
  const createUserProxy = useMutation(CreateUserProxyDocument)[1];

  const [userQuery] = useQuery({ query: UserListDocument });
  const userOptions = React.useMemo(() => {
    return (userQuery.data?.users?.nodes || []).map(x => ({
      id: x.id,
      label: `${x.uEmail}, ${x.uLogin}`,
    }));
  }, [userQuery]);

  if (!auth.isAdmin) return;

  return (
    <Popover open={open} onOpenChange={setOpen}>
      <PopoverTrigger asChild>
        <button type="button" className={buttonCls({ size: 'xs', variant: 'outline' })}>
          <Plus /> Přidat k registrovanému uživateli
        </button>
      </PopoverTrigger>
      <PopoverPrimitive.Portal>
        <PopoverPrimitive.Content className="z-40" align="end" side='top' sideOffset={5}>
          <ComboboxSearchArea
            value={null}
            onChange={(id) => {
              if (id) createUserProxy({ input: { userProxy: { personId: person.id, userId: id } } });
              setOpen(false);
            }}
            options={userOptions}
          />
        </PopoverPrimitive.Content>
      </PopoverPrimitive.Portal>
    </Popover>
  );
}
