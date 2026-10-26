import Image from '@/ui/Image';
import { Users } from 'lucide-react';
import { cn } from '@/lib/cn';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { TextField } from '@/ui/fields/text';
import { CohortForm } from '@/ui/forms/CohortForm';
import { buttonCls } from '@/ui/style';
import { useAuth } from '@/lib/auth';
import { useFuzzySearch } from '@/lib/useFuzzySearch';
import Link from 'next/link';
import { useParams } from 'next/navigation';
import React from 'react';
import { useQuery } from 'urql';
import { CohortListDocument } from '@/graphql/Cohorts';
import { slugify } from '@/lib/slugify';
import { formatCount } from '@/ui/format';

export function CohortList() {
  const currentId = useParams<{ id?: string }>()?.id;
  const auth = useAuth();
  const [isArchive, setIsArchive] = React.useState(false);
  const [isHidden, setIsHidden] = React.useState(false);
  const [{ data: cohorts }] = useQuery({
    query: CohortListDocument,
    variables: { visible: !isHidden, archived: isArchive },
  });
  const nodes = React.useMemo(
    () =>
      cohorts?.cohortsList?.map((x) => {
        const count = x.cohortMembershipsList.length;
        return {
          id: x.id,
          image: x.image,
          title: x.name,
          subtitle: [
            x.location,
            formatCount(count, { one: 'člen', few: 'členové', other: 'členů' }),
            !x.isVisible && 'Skrytá',
            x.isArchived && 'Archivovaná',
          ]
            .filter(Boolean)
            .join(', '),
          colorRgb: x.colorRgb,
          href: `/treninkove-skupiny/${x.id}/${slugify(x.name)}`,
        };
      }) || [],
    [cohorts?.cohortsList],
  );
  const [search, setSearch] = React.useState('');
  const fuzzy = useFuzzySearch(nodes, ['id', 'title'], search);

  return (
    <div className="flex flex-col h-full">
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Skupiny</div>

        {auth.isAdmin && (
          <Dialog>
            <DialogTrigger.Add size="sm" />
            <DialogContent>
              <CohortForm />
            </DialogContent>
          </Dialog>
        )}

        {auth.isTrainer && (
          <div className="mt-2 w-full flex gap-2 justify-end">
            <button
              type="button"
              className={buttonCls({
                size: 'sm',
                variant: isArchive ? 'primary' : 'outline',
              })}
              onClick={() => setIsArchive((x) => !x)}
            >
              Archivované
            </button>
            <button
              type="button"
              className={buttonCls({
                size: 'sm',
                variant: isHidden ? 'primary' : 'outline',
              })}
              onClick={() => setIsHidden((x) => !x)}
            >
              Skryté
            </button>
          </div>
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
              className: 'm-1 mt-0 flex items-center gap-2 rounded-xl p-1.5 pl-6 text-sm',
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
            <div
              className={cn(
                'truncate text-sm',
                currentId === item.id ? 'text-white' : 'text-neutral-11',
              )}
            >
                {item.subtitle}
              </div>
            </div>
            <div
              className="absolute rounded-l-xl w-4 shadow-xs inset-y-0 left-0"
              style={{ backgroundColor: item.colorRgb }}
            />
          </Link>
        ))}
      </div>
    </div>
  );
}
