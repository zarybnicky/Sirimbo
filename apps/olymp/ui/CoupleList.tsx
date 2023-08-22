import { useRouter } from 'next/router';
import Link from 'next/link';
import { useQuery } from 'urql';
import { fromSlugArray } from '@app/ui/slugify';
import { CoupleListDocument } from '@app/graphql/Couple';
import React from 'react';
import { NewCoupleDialog } from './NewCoupleDialog';
import { useAuth } from './use-auth';
import { useFuzzySearch } from '@app/ui/use-fuzzy-search';
import { TextField } from '@app/ui/fields/text';
import { cn } from './cn';
import { formatCoupleName } from './format-name';

export function CoupleList() {
  const router = useRouter();
  const { perms } = useAuth();

  const [{ data }] = useQuery({ query: CoupleListDocument });
  const id = fromSlugArray(router.query.id);

  const nodes = React.useMemo(() => {
    return (data?.couples?.nodes || []).map((item) => ({
      id: item.id,
      title: formatCoupleName(item),
    }));
  }, [data]);

  const [search, setSearch] = React.useState('');
  const fuzzy = useFuzzySearch(nodes, ['id', 'title'], search);

  return (
    <>
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Páry</div>

        {perms.isAdmin && (
          <NewCoupleDialog />
        )}

        <TextField
          type="search"
          className="w-full mt-2"
          placeholder="Vyhledat..."
          value={search}
          onChange={(e) => setSearch(e.currentTarget.value)}
        />
      </div>

      {fuzzy.map((item) => (
        <Link
          key={item.id}
          href={`/pary/${item.id}`}
          className={cn(
            'relative p-2 mr-2 my-1 rounded-lg grid',
            id === item.id
              ? 'font-semibold bg-primary text-white shadow-md'
              : 'hover:bg-neutral-4',
          )}
        >
          <div>{item.title}</div>
        </Link>
      ))}
    </>
  );
}
