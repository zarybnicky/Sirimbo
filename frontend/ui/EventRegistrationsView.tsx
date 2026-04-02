import { type EventFullFragment } from '@/graphql/Event';
import { type ResolvedActions, useActionMap } from '@/lib/actions';
import {
  type EventExternalRegistrationActionItem,
  eventExternalRegistrationActions,
  type EventRegistrationActionItem,
  eventRegistrationActions,
} from '@/lib/actions/event';
import { ActionGroup } from '@/ui/ActionGroup';
import { Popover, PopoverContent, PopoverTrigger } from '@/ui/popover';
import { buttonCls } from '@/ui/style';
import { type Column, DataGrid, type SortColumn } from 'react-data-grid';
import {
  dateTimeFormatter,
  formatLongCoupleName,
  formatRegistrant,
} from '@/ui/format';
import { exportEventRegistrations } from '@/ui/reports/export-event-registrations';
import Link from 'next/link';
import React from 'react';

type InternalRegistration = EventFullFragment['eventRegistrationsList'][number];
type ExternalRegistration = EventFullFragment['eventExternalRegistrationsList'][number];

type RegistrationRow = {
  id: string;
  kind: 'internal' | 'external';
  registrantLabel: string;
  registeredAt: string;
  note: string;
  lessonDemands: string[];
  lessonDemandCount: number;
  lessonDemandTotal: number;
  searchText: string;
  internalRegistration: InternalRegistration | null;
  externalRegistration: ExternalRegistration | null;
};

function getRegistrationRows(event: EventFullFragment): RegistrationRow[] {
  const internalRows = event.eventRegistrationsList.map((registration) => {
    const lessonDemands = registration.eventLessonDemandsByRegistrationIdList
      .map((demand) => {
        const trainer = event.eventTrainersList.find((item) => item.id === demand.trainerId);
        return `${demand.lessonCount}x ${trainer?.name || '?'}`;
      });
    const lessonDemandTotal = registration.eventLessonDemandsByRegistrationIdList.reduce(
      (sum, demand) => sum + demand.lessonCount,
      0,
    );

    const registrantLabel = formatRegistrant(registration);

    return {
      id: `internal:${registration.id}`,
      kind: 'internal' as const,
      registrantLabel,
      registeredAt: registration.createdAt,
      note: registration.note || '',
      lessonDemands,
      lessonDemandCount: lessonDemands.length,
      lessonDemandTotal,
      searchText: `${registrantLabel} ${registration.note || ''} ${lessonDemands.join(' ')}`.toLowerCase(),
      internalRegistration: registration,
      externalRegistration: null,
    };
  });

  const externalRows = event.eventExternalRegistrationsList.map((registration) => {
    const registrantLabel = [
      registration.prefixTitle,
      registration.firstName,
      registration.lastName,
      registration.suffixTitle,
    ]
      .filter(Boolean)
      .join(' ');

    return {
      id: `external:${registration.id}`,
      kind: 'external' as const,
      registrantLabel,
      registeredAt: registration.createdAt,
      note: registration.note || '',
      lessonDemands: [],
      lessonDemandCount: 0,
      lessonDemandTotal: 0,
      searchText: `${registrantLabel} ${registration.email} ${registration.phone} ${registration.note || ''}`.toLowerCase(),
      internalRegistration: null,
      externalRegistration: registration,
    };
  });

  return [...internalRows, ...externalRows];
}

function sortRows(rows: RegistrationRow[], sortColumns: readonly SortColumn[]) {
  if (sortColumns.length === 0) {
    return rows.toSorted((a, b) => b.registeredAt.localeCompare(a.registeredAt));
  }

  const { columnKey, direction } = sortColumns[0]!;
  const dir = direction === 'ASC' ? 1 : -1;

  return rows.toSorted((left, right) => {
    let cmp = 0;

    switch (columnKey) {
      case 'registrant':
        cmp = left.registrantLabel.localeCompare(right.registrantLabel, 'cs');
        break;
      case 'kind':
        cmp = left.kind.localeCompare(right.kind, 'cs');
        break;
      case 'registeredAt':
        cmp = left.registeredAt.localeCompare(right.registeredAt);
        break;
      case 'lessonDemands':
        cmp =
          left.lessonDemandTotal - right.lessonDemandTotal ||
          left.lessonDemandCount - right.lessonDemandCount;
        break;
      case 'note':
        cmp = left.note.localeCompare(right.note, 'cs');
        break;
      default:
        cmp = 0;
        break;
    }

    return cmp * dir;
  });
}

function RegistrationRegistrantCell({ row }: { row: RegistrationRow }) {
  if (row.internalRegistration?.person) {
    return (
      <Link
        href={{ pathname: '/clenove/[id]', query: { id: row.internalRegistration.person.id } }}
        className="font-medium text-accent-11 hover:underline"
      >
        {row.registrantLabel}
      </Link>
    );
  }

  if (row.internalRegistration?.couple) {
    return (
      <Link
        href={{ pathname: '/pary/[id]', query: { id: row.internalRegistration.couple.id } }}
        className="font-medium text-accent-11 hover:underline"
      >
        {formatLongCoupleName(row.internalRegistration.couple)}
      </Link>
    );
  }

  return <span className="font-medium text-neutral-12">{row.registrantLabel}</span>;
}

function RegistrationTypeCell({ kind }: { kind: RegistrationRow['kind'] }) {
  return (
    <span className="text-sm text-neutral-11">
      {kind === 'internal' ? 'Interní' : 'Externí'}
    </span>
  );
}

function LessonDemandsCell({ row }: { row: RegistrationRow }) {
  if (row.lessonDemandCount === 0) {
    return <span className="text-sm text-neutral-11">—</span>;
  }

  const summary =
    row.lessonDemandCount === 1
      ? row.lessonDemands[0]!
      : `${row.lessonDemandTotal} lekcí u ${row.lessonDemandCount} trenérů`;

  if (row.lessonDemandCount === 1) {
    return <span className="text-sm text-neutral-12">{summary}</span>;
  }

  return (
    <Popover>
      <PopoverTrigger asChild>
        <button
          type="button"
          className="text-left text-sm text-accent-11 underline underline-offset-2"
        >
          {summary}
        </button>
      </PopoverTrigger>
      <PopoverContent align="start" className="w-80 max-w-[calc(100vw-2rem)] p-4">
        <div className="space-y-2 pr-6">
          <div className="text-sm font-semibold text-neutral-12">Požadavky na lekce</div>
          <ul className="space-y-1 text-sm text-neutral-12">
            {row.lessonDemands.map((lessonDemand) => (
              <li key={lessonDemand}>{lessonDemand}</li>
            ))}
          </ul>
        </div>
      </PopoverContent>
    </Popover>
  );
}

function RegistrationActionsCell({
  row,
  internalActionMap,
  externalActionMap,
}: {
  row: RegistrationRow;
  internalActionMap: Map<string, ResolvedActions<EventRegistrationActionItem>>;
  externalActionMap: Map<string, ResolvedActions<EventExternalRegistrationActionItem>>;
}) {
  if (row.internalRegistration) {
    const actions = internalActionMap.get(`internal:${row.internalRegistration.id}`);
    return actions ? <ActionGroup variant="row" actions={actions} /> : null;
  }

  if (row.externalRegistration) {
    const actions = externalActionMap.get(`external:${row.externalRegistration.id}`);
    return actions ? <ActionGroup variant="row" actions={actions} /> : null;
  }

  return null;
}

export function EventRegistrationsView({
  event,
}: {
  event: EventFullFragment;
}) {
  const [query, setQuery] = React.useState('');
  const [kind, setKind] = React.useState<'all' | 'internal' | 'external'>('all');
  const [sortColumns, setSortColumns] = React.useState<SortColumn[]>([
    { columnKey: 'registeredAt', direction: 'DESC' },
  ]);

  const baseRows = React.useMemo(() => getRegistrationRows(event), [event]);
  const filteredRows = React.useMemo(() => {
    const normalizedQuery = query.trim().toLowerCase();

    return baseRows.filter((row) => {
      if (kind !== 'all' && row.kind !== kind) return false;
      if (!normalizedQuery) return true;
      return row.searchText.includes(normalizedQuery);
    });
  }, [baseRows, kind, query]);
  const rows = React.useMemo(
    () => sortRows(filteredRows, sortColumns),
    [filteredRows, sortColumns],
  );
  const internalActionMap = useActionMap(
    eventRegistrationActions,
    event.eventRegistrationsList.map((registration) => ({
      id: `internal:${registration.id}`,
      event,
      registration,
    })),
  );
  const externalActionMap = useActionMap(
    eventExternalRegistrationActions,
    event.eventExternalRegistrationsList.map((registration) => ({
      id: `external:${registration.id}`,
      event,
      registration,
    })),
  );

  const columns = React.useMemo<Column<RegistrationRow>[]>(
    () => [
      {
        key: 'actions',
        name: '',
        width: 32,
        resizable: false,
        renderCell: ({ row }) => (
          <RegistrationActionsCell
            row={row}
            internalActionMap={internalActionMap}
            externalActionMap={externalActionMap}
          />
        ),
      },
      {
        key: 'registrant',
        name: 'Přihlášený',
        sortable: true,
        renderCell: ({ row }) => <RegistrationRegistrantCell row={row} />,
      },
      {
        key: 'kind',
        name: 'Typ',
        sortable: true,
        renderCell: ({ row }) => <RegistrationTypeCell kind={row.kind} />,
      },
      {
        key: 'registeredAt',
        name: 'Přihlášeno',
        sortable: true,
        renderCell: ({ row }) => (
          <time dateTime={row.registeredAt} className="text-sm text-neutral-12">
            {dateTimeFormatter.format(new Date(row.registeredAt))}
          </time>
        ),
      },
      {
        key: 'lessonDemands',
        name: 'Lekce',
        sortable: true,
        minWidth: 180,
        renderCell: ({ row }) => <LessonDemandsCell row={row} />,
      },
      {
        key: 'note',
        name: 'Poznámka',
        sortable: true,
        minWidth: 200,
        renderCell: ({ row }) => (
          <span className="truncate text-sm text-neutral-11">{row.note || '—'}</span>
        ),
      },
    ],
    [externalActionMap, internalActionMap],
  );

  const total = baseRows.length;

  return (
    <>
      <div className="mb-3 flex flex-wrap items-center gap-2">
        <input
          type="search"
          value={query}
          onChange={(event_) => setQuery(event_.currentTarget.value)}
          placeholder="Hledat podle jména, poznámky, e-mailu..."
          className="min-w-64 rounded-md border border-neutral-6 bg-neutral-1 px-3 py-2 text-sm text-neutral-12 placeholder:text-neutral-9 focus:border-accent-8 focus:outline-none"
        />
        <select
          value={kind}
          onChange={(event_) =>
            setKind(event_.currentTarget.value as 'all' | 'internal' | 'external')
          }
          className="rounded-md border border-neutral-6 bg-neutral-1 px-3 py-2 text-sm text-neutral-12"
        >
          <option value="all">Vše</option>
          <option value="internal">Interní</option>
          <option value="external">Externí</option>
        </select>
        <span className="text-sm text-neutral-11">
          {rows.length === total ? `${rows.length} řádků` : `${rows.length} z ${total}`}
        </span>
      </div>

      {rows.length === 0 ? (
        <div className="rounded-md border border-neutral-6 bg-neutral-2 p-4 text-sm text-neutral-11">
          Žádné přihlášky.
        </div>
      ) : (
        <>
          <div className="hidden overflow-auto rounded-md border border-neutral-6 bg-neutral-1 lg:block">
            <DataGrid
              columns={columns}
              rows={rows}
              rowKeyGetter={(row) => row.id}
              sortColumns={sortColumns}
              onSortColumnsChange={setSortColumns}
              defaultColumnOptions={{ resizable: true }}
              headerRowHeight={40}
              rowHeight={30}
              style={{ height: `${Math.min(rows.length * 5 + 9, 70)}vh` }}
            />
          </div>

          <div className="grid gap-2 lg:hidden">
            {rows.map((row) => (
              <div
                key={row.id}
                className="rounded-md border border-neutral-6 bg-neutral-1 px-3 py-2"
              >
                <div className="flex items-start justify-between gap-2">
                  <div className="min-w-0">
                    <RegistrationRegistrantCell row={row} />
                    <div className="text-sm text-neutral-11">
                      <RegistrationTypeCell kind={row.kind} /> {' · '}
                      {dateTimeFormatter.format(new Date(row.registeredAt))}
                    </div>
                  </div>
                  <RegistrationActionsCell
                    row={row}
                    internalActionMap={internalActionMap}
                    externalActionMap={externalActionMap}
                  />
                </div>

                {row.lessonDemandCount > 0 && (
                  <div className="mt-1">
                    <LessonDemandsCell row={row} />
                  </div>
                )}
                {row.note && <div className="mt-1 text-sm text-neutral-11">{row.note}</div>}
                {row.externalRegistration && (
                  <div className="mt-1 text-sm text-neutral-12">
                    <div>{row.externalRegistration.email}</div>
                    <div className="text-neutral-11">{row.externalRegistration.phone}</div>
                  </div>
                )}
              </div>
            ))}
          </div>
        </>
      )}
    </>
  );
}
