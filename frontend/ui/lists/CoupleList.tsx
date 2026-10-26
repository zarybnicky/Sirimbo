import Image from '@/ui/Image';
import { Users } from 'lucide-react';
import { CoupleListDocument } from '@/graphql/Memberships';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { TextField } from '@/ui/fields/text';
import { formatCoupleName } from '@/ui/format';
import { formatAgeGroup } from '@/ui/csts';
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
      image: item.image,
      title: formatCoupleName(item),
      ageGroup: formatAgeGroup(item.ageGroup),
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
              size: 'none',
              className: 'm-1 mt-0 flex items-center gap-2 rounded-xl p-1.5 px-2 text-sm',
            })}
          >
            <span className="flex size-10 shrink-0 items-center justify-center overflow-hidden rounded-md bg-neutral-3 text-neutral-10">
              {item.image ? (
                <Image
                  src={item.image.url}
                  alt=""
                  width={40}
                  height={40}
                  className="size-10 object-cover"
                />
              ) : (
                <Users className="size-5" aria-hidden="true" />
              )}
            </span>
            <div className="min-w-0 flex-1">
              <div className="truncate">{item.title}</div>
              {item.ageGroup && <div className="truncate text-xs">{item.ageGroup}</div>}
            </div>
          </Link>
        ))}
      </div>
    </div>
  );
}
