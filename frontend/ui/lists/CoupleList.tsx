import { ListItem } from '@/ui/ListItem';
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

export function CoupleList() {
  const currentId = useParams<{ id?: string }>()?.id;
  const auth = useAuth();

  const [{ data }] = useQuery({ query: CoupleListDocument });

  const nodes = React.useMemo(() => {
    return (data?.couplesList || []).map((item) => {
      const cohorts = [...new Map(
        [item.man, item.woman]
          .flatMap((person) => person?.cohortMembershipsList ?? [])
          .flatMap((membership) => membership.cohort
            ? [[membership.cohort.id, membership.cohort] as const]
            : []),
      ).values()];
      return {
        id: item.id,
        image: item.image,
        title: formatCoupleName(item),
        ageGroup: formatAgeGroup(item.ageGroup),
        href: `/pary/${item.id}`,
        cohortNames: cohorts.map((cohort) => cohort.name).join(', '),
        cohortColors: cohorts.map((cohort) => cohort.colorRgb),
      };
    });
  }, [data]);

  const [search, setSearch] = React.useState('');
  const fuzzy = useFuzzySearch(nodes, ['id', 'title', 'cohortNames'], search);

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
          <ListItem
            key={item.id}
            href={item.href}
            title={item.title}
            subtitle={item.ageGroup}
            selected={currentId === item.id}
            imageUrl={item.image?.url}
            icon={<Users className="size-5" />}
            colors={item.cohortColors}
          />
        ))}
      </div>
    </div>
  );
}
