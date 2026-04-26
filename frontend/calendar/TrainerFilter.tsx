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
import React from 'react';
import { isTruthy } from '@/lib/truthyFilter';

export function TrainerFilter() {
  const [trainerIds, setTrainerIds] = useAtom(trainerIdsFilterAtom);
  const [{ data: tenant }] = useQuery({ query: CurrentTenantDocument });
  const availableTrainers = (tenant?.tenant?.tenantTrainersList || [])
    .map((x) => x.person)
    .filter(isTruthy);

  return (
    <DropdownMenu>
      <DropdownMenuTrigger className={buttonCls({ variant: 'outline', size: 'sm' })}>
        <Filter />
      </DropdownMenuTrigger>
      <DropdownMenuContent>
        {availableTrainers.map((x) => (
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
