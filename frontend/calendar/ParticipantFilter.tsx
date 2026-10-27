import { useQueryState } from 'nuqs';
import { calendarFilterParsers } from '@/calendar/state';
import { useQuery } from 'urql';
import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { buttonCls } from '@/ui/style';
import { CheckIcon, Filter } from 'lucide-react';
import { isTruthy } from '@/lib/truthyFilter';
import { compareText } from '@/lib/compare';
import { PersonOptionsDocument } from '@/graphql/Person';

export function ParticipantFilter({ counts }: { counts?: Record<string, number> }) {
  const [ids, setIds] = useQueryState('participants', calendarFilterParsers.participants);
  const [{ data }] = useQuery({
    query: PersonOptionsDocument,
    variables: { condition: { membership: { state: 'CURRENT' } } },
  });
  const available = (data?.people?.nodes || [])
    .filter(isTruthy)
    .toSorted((a, b) =>
      compareText(a.lastName, b.lastName) || compareText(a.firstName, b.firstName),
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
            <span className="ml-auto tabular-nums text-neutral-11">
              {counts ? (counts[x.id] ?? 0) : '…'}
            </span>
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
            <span className="ml-auto tabular-nums text-neutral-11">
              {counts ? (counts[x.id] ?? 0) : '…'}
            </span>
          </DropdownMenuButton>
        ))}
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
