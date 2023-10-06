import { CreateUserProxyDocument } from "@/graphql/Memberships";
import { PersonFragment } from "@/graphql/Person";
import React from "react";
import { useMutation, useQuery } from "urql";
import { UserListDocument } from "@/graphql/CurrentUser";
import { ComboboxSearchArea } from "./Combobox";
import { Popover, PopoverTrigger } from '@/ui/popover';
import * as PopoverPrimitive from '@radix-ui/react-popover';
import { Plus } from 'lucide-react';
import { buttonCls } from "./style";

export function AddToPersonButton({ person }: { person: PersonFragment; onSuccess?: () => void }) {
  const [open, setOpen] = React.useState(false);
  const createUserProxy = useMutation(CreateUserProxyDocument)[1];

  const [userQuery] = useQuery({ query: UserListDocument });
  const userOptions = React.useMemo(() => {
    return (userQuery.data?.users?.nodes || []).map(x => ({
      id: x.id,
      label: `${x.uEmail}, ${x.uLogin}`,
    }));
  }, [userQuery]);

  return (
    <Popover open={open} onOpenChange={setOpen}>
      <PopoverTrigger asChild>
        <button type="button" className={buttonCls({ size: 'xs', variant: 'outline' })}>
          <Plus /> Trenér
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
