import { useAtom } from 'jotai';
import { participantIdsFilterAtom } from '@/calendar/state';
import { useQuery } from 'urql';
import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { buttonCls } from '@/ui/style';
import { CheckIcon, Filter } from 'lucide-react';
import React from 'react';
import { isTruthy } from '@/lib/truthyFilter';
import { PersonListDocument } from '@/graphql/Person';

export function ParticipantFilter() {
  const [ids, setIds] = useAtom(participantIdsFilterAtom);
  const [{ data }] = useQuery({
    query: PersonListDocument,
    variables: {
      inCohorts: null,
      isAdmin: null,
      isTrainer: null,
      membershipState: 'current',
    },
  });
  const available = (data?.filteredPeopleList || [])
    .filter(isTruthy)
    .toSorted((a, b) =>
      `${a.lastName} ${a.firstName}`.localeCompare(`${b.lastName} ${b.firstName}`),
    );
  const selected = available.filter((x) => ids.includes(x.id));
  const other = available.filter((x) => !ids.includes(x.id));

  return (
    <DropdownMenu>
      <DropdownMenuTrigger className={buttonCls({ variant: 'outline', size: 'sm' })}>
        <Filter />
        Účastníci
      </DropdownMenuTrigger>
      <DropdownMenuContent>
        {selected.map((x) => (
          <DropdownMenuButton
            key={x.id}
            onSelect={(e) => {
              e.preventDefault();
              setIds((xs) => xs.filter((y) => y !== x.id));
            }}
          >
            <CheckIcon />
            {x.name}
          </DropdownMenuButton>
        ))}
        {other.map((x) => (
          <DropdownMenuButton
            key={x.id}
            onSelect={(e) => {
              e.preventDefault();
              setIds((xs) => [...xs, x.id]);
            }}
          >
            {x.name}
          </DropdownMenuButton>
        ))}
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
