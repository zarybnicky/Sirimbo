import { useQueryState } from 'nuqs';
import { calendarFilterParsers } from '@/calendar/state';
import { useQuery } from 'urql';
import { TrainerOptionsDocument } from '@/graphql/Tenant';
import { DropdownMenuButton } from '@/ui/dropdown';
import { FacetFilter } from '@/ui/FacetFilter';
import { CheckCircle2, Circle } from 'lucide-react';
import { isTruthy } from '@/lib/truthyFilter';
import { compareText } from '@/lib/compare';

export type TrainerFilterOption = {
  id: string;
  name: string;
};

export function TrainerFilter({
  counts,
  availableTrainers,
  showAllResources,
  onShowAllResourcesChange,
}: {
  counts?: Record<string, number>;
  availableTrainers?: readonly TrainerFilterOption[];
  showAllResources?: boolean;
  onShowAllResourcesChange?: (showAllResources: boolean) => void;
}) {
  const [trainerIds, setTrainerIds] = useQueryState(
    'trainers',
    calendarFilterParsers.trainers,
  );
  const [{ data: tenant }] = useQuery({
    query: TrainerOptionsDocument,
    pause: availableTrainers !== undefined,
  });
  const trainers =
    availableTrainers?.toSorted((a, b) => compareText(a.name, b.name)) ??
    (tenant?.tenant?.tenantTrainersList || [])
      .map((x) => x.person)
      .filter(isTruthy)
      .toSorted(
        (a, b) =>
          compareText(a.lastName, b.lastName) || compareText(a.firstName, b.firstName),
      );

  return (
    <FacetFilter
      label="Trenéři"
      value={trainerIds}
      options={trainers
        .filter(({ id }) => showAllResources || !!counts?.[id] || trainerIds.includes(id))
        .map(({ id, name }) => ({ value: id, label: name, count: counts?.[id] }))}
      onChange={setTrainerIds}
    >
      {onShowAllResourcesChange && (
        <DropdownMenuButton
          role="menuitemcheckbox"
          aria-checked={!!showAllResources}
          onSelect={(e) => {
            e.preventDefault();
            onShowAllResourcesChange(!showAllResources);
          }}
        >
          {showAllResources ? <CheckCircle2 /> : <Circle />}
          Zobrazit všechny trenéry
        </DropdownMenuButton>
      )}
    </FacetFilter>
  );
}
