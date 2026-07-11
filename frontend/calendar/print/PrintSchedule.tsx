import React from 'react';
import { Printer } from 'lucide-react';
import { parseAsIsoDate, parseAsStringLiteral, useQueryState } from 'nuqs';
import { CalendarViews } from '@/calendar/CalendarViews';
import { useCalendarData } from '@/calendar/useCalendarData';
import { CalendarDatePicker } from '@/calendar/CalendarDatePicker';
import { useAuth } from '@/ui/use-auth';
import { buttonCls } from '@/ui/style';
import { Spinner } from '@/ui/Spinner';
import { instanceEvents } from './model';
import { PrintDay } from './PrintDay';
import { PrintWeek } from './PrintWeek';
import { PrintMonth } from './PrintMonth';

const printViewKeys = ['day', 'week', 'month'] as const;
type PrintViewKey = (typeof printViewKeys)[number];

const viewLabels: Record<PrintViewKey, string> = {
  day: 'Den',
  week: 'Týden',
  month: 'Měsíc',
};

/**
 * Standalone, print-ready schedule page. Reuses the calendar's data hook but
 * renders static, paper-friendly layouts instead of the interactive grid.
 */
export function PrintSchedule() {
  const auth = useAuth();
  const today = React.useMemo(() => new Date(), []);
  const [viewKey, setViewKey] = useQueryState(
    'v',
    parseAsStringLiteral(printViewKeys).withDefault('day'),
  );
  const [nullableDate, setDate] = useQueryState('d', parseAsIsoDate.withDefault(today));
  const date = nullableDate ?? today;

  const view = CalendarViews[viewKey];
  const filters = React.useMemo(
    () => ({
      onlyMine: false,
      trainerIds: [],
      participantIds: [],
      myPersonIds: auth.personIds,
    }),
    [auth.personIds],
  );

  const { fetching, range, events } = useCalendarData(view, date, filters, 'none');
  const instances = React.useMemo(() => instanceEvents(events), [events]);

  return (
    <div className="print-schedule mx-auto w-full max-w-[1100px] p-4 text-neutral-12">
      <div className="print:hidden mb-4 flex flex-wrap items-center gap-2">
        <CalendarDatePicker date={date} setDate={setDate} view={view} />

        <div className="inline-flex overflow-hidden rounded-xl border border-accent-6 shadow-md">
          {printViewKeys.map((key) => (
            <button
              key={key}
              type="button"
              onClick={() => setViewKey(key)}
              className={buttonCls({
                variant: viewKey === key ? 'primary' : 'outline',
                display: 'none',
                className: 'rounded-none px-3 py-2 text-sm font-medium uppercase',
              })}
            >
              {viewLabels[key]}
            </button>
          ))}
        </div>

        <button
          type="button"
          onClick={() => window.print()}
          className={buttonCls({ variant: 'primary', className: 'gap-2' })}
        >
          <Printer /> Tisk
        </button>

        {fetching && <Spinner />}
        <span className="grow text-right text-lg">{view.label(range)}</span>
      </div>

      <div className="hidden print:mb-3 print:block text-xl font-semibold">
        {view.label(range)}
      </div>

      {viewKey === 'day' && <PrintDay date={date} events={instances} />}
      {viewKey === 'week' && <PrintWeek range={range} events={instances} />}
      {viewKey === 'month' && <PrintMonth anchor={date} events={instances} />}
    </div>
  );
}
