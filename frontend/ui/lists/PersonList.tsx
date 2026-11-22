import { ListItem } from '@/ui/ListItem';
import { UserRound } from 'lucide-react';
import { TextField } from '@/ui/fields/text';
import { useClient } from 'urql';
import { ComboboxButton } from '@/ui/fields/Combobox';
import { FacetFilter } from '@/ui/FacetFilter';
import { CreatePersonDialog } from '@/ui/CreatePersonDialog';
import { useAuth } from '@/lib/auth';
import { buttonCls } from '@/ui/style';
import { useParams, useSearchParams } from 'next/navigation';
import { useQueryStates } from 'nuqs';
import { personFilterParsers, usePersonData } from '@/lib/usePersonData';
import { FormError } from '@/ui/form';
import { exportNsa } from '../reports/export-nsa';

export function PersonList() {
  const id = useParams<{ id?: string }>()?.id;
  const auth = useAuth();
  const client = useClient();

  const [filters, setFilters] = useQueryStates(personFilterParsers, { history: 'push' });
  const { people, filterOptions, error } = usePersonData({ filters });
  const searchParams = useSearchParams();
  const flags = [
    ...(filters.membership === 'former' ? ['former'] : []),
    ...(filters.isTrainer ? ['trainer'] : []),
    ...(filters.isAdmin ? ['admin'] : []),
  ];

  return (
    <div className="flex flex-col h-full">
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Členové</div>
        <div className="flex gap-2 justify-end">
          {auth.isAdmin && (
            <button
              type="button"
              className={buttonCls({ size: 'xs' })}
              onClick={() => exportNsa(client)}
            >
              NSA export
            </button>
          )}
          {auth.isAdmin && <CreatePersonDialog />}
        </div>

        <div className="mt-2 w-full flex flex-wrap gap-2 justify-end">
          <ComboboxButton
            value={filters.cohortId}
            onChange={(cohortId) => setFilters({ cohortId })}
            placeholder="skupina"
            options={filterOptions.cohorts}
          />

          <FacetFilter
            inline
            label=""
            value={flags}
            options={filterOptions.flags}
            onChange={(values) =>
              setFilters({
                membership: values.includes('former') ? 'former' : 'current',
                isTrainer: values.includes('trainer'),
                isAdmin: values.includes('admin'),
              })
            }
          />
        </div>

        <TextField
          type="search"
          className="w-full mt-2"
          placeholder="Jméno, email, telefon"
          value={filters.search}
          onChange={(e) =>
            setFilters({ search: e.currentTarget.value }, { history: 'replace' })
          }
        />
      </div>

      <div className="grow h-full overflow-y-auto scrollbar">
        <FormError error={error} />
        {people.map((item) => (
          <ListItem
            key={item.id}
            href={`/clenove/${item.id}${searchParams?.size ? `?${searchParams}` : ''}`}
            title={item.name}
            subtitle={[
              item.yearOfBirth,
              item.roles.includes('ADMINISTRATOR') ? 'Správce' : '',
              item.roles.includes('TRAINER') ? 'Trenér' : '',
              item.roles.includes('MEMBER') ? 'Člen' : '',
              ...(item.couplesList ?? []).flatMap((x) =>
                [x.man, x.woman]
                  .filter((x) => x?.id !== item.id)
                  .map((x) => `tančí s ${x?.name}`),
              ),
            ]
              .filter(Boolean)
              .join(', ')}
            selected={id === item.id}
            imageUrl={item.image?.url}
            avatarName={item.name}
            icon={<UserRound className="size-5" />}
            colors={item.cohortColors}
          />
        ))}
      </div>
    </div>
  );
}
