import { useAtom } from 'jotai';
import { trainerIdsFilterAtom } from '@/calendar/state';
import { useQuery } from 'urql';
import { CurrentTenantDocument } from '@/graphql/Tenant';
import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { buttonCls } from '@/ui/style';
import { CheckCircle2, Circle, Filter } from 'lucide-react';
import { isTruthy } from '@/lib/truthyFilter';

export type TrainerFilterOption = {
  id: string;
  name: string;
};

export function TrainerFilter({
  availableTrainers,
  showAllResources,
  onShowAllResourcesChange,
}: {
  availableTrainers?: readonly TrainerFilterOption[];
  showAllResources?: boolean;
  onShowAllResourcesChange?: (showAllResources: boolean) => void;
}) {
  const [trainerIds, setTrainerIds] = useAtom(trainerIdsFilterAtom);
  const [{ data: tenant }] = useQuery({
    query: CurrentTenantDocument,
    pause: availableTrainers !== undefined,
  });
  const trainers =
    availableTrainers?.toSorted((a, b) => a.name.localeCompare(b.name)) ??
    (tenant?.tenant?.tenantTrainersList || [])
      .map((x) => x.person)
      .filter(isTruthy)
      .toSorted((a, b) =>
        `${a.lastName} ${a.firstName}`.localeCompare(`${b.lastName} ${b.firstName}`),
      );

  return (
    <DropdownMenu>
      <DropdownMenuTrigger className={buttonCls({ variant: 'outline', size: 'sm' })}>
        <Filter />
        Trenéři
      </DropdownMenuTrigger>
      <DropdownMenuContent>
        {onShowAllResourcesChange && (
          <DropdownMenuButton
            onSelect={(e) => {
              e.preventDefault();
              onShowAllResourcesChange(!showAllResources);
            }}
          >
            {showAllResources ? <CheckCircle2 /> : <Circle />}
            Zobrazit všechny trenéry
          </DropdownMenuButton>
        )}
        {trainers.map((x) => (
          <DropdownMenuButton
            key={x.id}
            onSelect={(e) => {
              e.preventDefault();
              setTrainerIds((xs) =>
                xs.includes(x.id) ? xs.filter((y) => y !== x.id) : [...xs, x.id],
              );
            }}
          >
            {trainerIds.includes(x.id) ? <CheckCircle2 /> : <Circle />}
            {x.name}
          </DropdownMenuButton>
        ))}
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
