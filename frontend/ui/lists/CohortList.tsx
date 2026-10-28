import { ListItem } from '@/ui/ListItem';
import { Users } from 'lucide-react';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { TextField } from '@/ui/fields/text';
import { CohortForm } from '@/ui/forms/CohortForm';
import { FacetFilter } from '@/ui/FacetFilter';
import { useAuth } from '@/lib/auth';
import { useFuzzySearch } from '@/lib/useFuzzySearch';
import { useParams } from 'next/navigation';
import React from 'react';
import { useQuery } from 'urql';
import { CohortListDocument } from '@/graphql/Cohorts';
import { slugify } from '@/lib/slugify';
import { formatCount } from '@/ui/format';

export function CohortList() {
  const currentId = useParams<{ id?: string }>()?.id;
  const auth = useAuth();
  const [showAll, setShowAll] = React.useState(false);
  const [{ data: cohorts }] = useQuery({
    query: CohortListDocument,
    variables: {
      visible: showAll ? undefined : true,
      archived: showAll ? undefined : false,
      withDetails: true,
    },
  });
  const nodes = React.useMemo(
    () =>
      cohorts?.cohortsList?.map((x) => {
        const count = x.cohortMembershipsList?.length ?? 0;
        return {
          id: x.id,
          image: x.image,
          title: x.name,
          subtitle: [
            x.location,
            formatCount(count, { one: 'člen', few: 'členové', other: 'členů' }),
            !x.isVisible && 'Neveřejná',
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

        <div className="flex gap-2">
          {auth.isTrainer && (
            <FacetFilter
              inline
              label=""
              value={showAll ? ['all'] : []}
              options={[{ value: 'all', label: 'Zobrazit vše' }]}
              onChange={(x) => setShowAll(x.includes('all'))}
            />
          )}
          {auth.isAdmin && (
            <Dialog>
              <DialogTrigger.Add size="sm" />
              <DialogContent>
                <CohortForm />
              </DialogContent>
            </Dialog>
          )}
        </div>

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
          <ListItem
            key={item.id}
            href={item.href}
            title={item.title}
            subtitle={item.subtitle}
            selected={currentId === item.id}
            imageUrl={item.image?.url}
            icon={<Users className="size-5" />}
            colors={[item.colorRgb]}
          />
        ))}
      </div>
    </div>
  );
}
