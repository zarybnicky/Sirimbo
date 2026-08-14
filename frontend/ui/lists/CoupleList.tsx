import { CoupleListDocument } from '@/graphql/Memberships';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { TextField } from '@/ui/fields/text';
import { formatCoupleName } from '@/ui/format';
import { CreateCoupleForm } from '@/ui/forms/CreateCoupleForm';
import { useAuth } from '@/lib/auth';
import { useFuzzySearch } from '@/lib/useFuzzySearch';
import { useParams } from 'next/navigation';
import React from 'react';
import { useQuery } from 'urql';
import Link from 'next/link';
import { buttonCls } from '@/ui/style';

export function CoupleList() {
  const currentId = useParams<{ id?: string }>()?.id;
  const auth = useAuth();

  const [{ data }] = useQuery({ query: CoupleListDocument });

  const nodes = React.useMemo(() => {
    return (data?.getCurrentTenant?.couplesList || []).map((item) => ({
      id: item.id,
      title: formatCoupleName(item),
      href: `/pary/${item.id}`,
    }));
  }, [data]);

  const [search, setSearch] = React.useState('');
  const fuzzy = useFuzzySearch(nodes, ['id', 'title'], search);

  return (
    <div className="flex flex-col h-full">
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Páry</div>

        {auth.isAdmin && (
          <Dialog modal={false}>
            <DialogTrigger.Add size="sm" text="Přidat pár" />
            <DialogContent>
              <CreateCoupleForm />
            </DialogContent>
          </Dialog>
        )}

        <TextField
          type="search"
          className="w-full mt-2"
          placeholder="Vyhledat..."
          value={search}
          onChange={(e) => setSearch(e.currentTarget.value)}
        />
      </div>

      <div className="grow h-full overflow-y-auto scrollbar">
        {fuzzy.map((item) => (
          <Link
            key={item.id}
            href={item.href}
            className={buttonCls({
              variant: currentId === item.id ? 'primary' : 'outline',
              display: 'none',
              className: 'pl-5 m-1 mt-0 grid',
            })}
          >
            <div>{item.title}</div>
          </Link>
        ))}
      </div>
    </div>
  );
}
