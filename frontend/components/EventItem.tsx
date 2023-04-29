import * as React from 'react';
import { Card } from 'components/Card';
import { fullDateFormatter } from 'lib/format-date';
import { Button } from './Button';
import { SimpleDialog } from './Dialog';
import { EventFragment, MyEventFragment } from 'lib/graphql/Event';
import { ParticipationDialog, ParticipationForm } from './ParticipationForm';
import { RichTextView } from './RichTextView';
import classNames from 'classnames';
import { usePermissions } from 'lib/data/use-permissions';
import { Dropdown } from './Dropdown';

interface Props {
  event: EventFragment & Partial<MyEventFragment>;
  expanded?: boolean;
}

export const EventItem = ({ event, expanded: expandedInit = false }: Props) => {
  const [expanded, setExpanded] = React.useState(expandedInit);
    const perms = usePermissions();
  const open = React.useCallback(() => setExpanded(true), []);

  return (
    <Card className="break-inside-avoid odd:bg-white even:bg-red-100/20">
      {perms.canEditCohort(event) && (
        <Dropdown
          className="absolute right-1 top-1"
          align="end"
          options={[{ title: 'Upravit', href: `/admin/akce/${event.id}` }]}
        />
      )}
      <div className="flex justify-between flex-wrap text-stone-600">
        <div>
          {fullDateFormatter.formatRange(
            new Date(event.since || ''),
            new Date(event.until || ''),
          )}
        </div>
        <div>
          Zbývá {event.remainingSpots} míst z {event.capacity}
        </div>
      </div>
      <div className="text-2xl text-stone-900">{event.name}</div>
      <div className="text-stone-600">{event.locationText}</div>

      <div
        className={classNames('mb-2', !expanded && 'cursor-pointer')}
        onClick={!expanded ? open : undefined}
      >
        <RichTextView value={event.summary} />

        {expanded ? (
          <RichTextView value={(event.description || '').replaceAll('\n', '<br/>')} />
        ) : (
          <div className="text-red-500 font-bold mt-3">Zobrazit více...</div>
        )}
      </div>
      {((event.attendeeUsers?.nodes?.length ?? 0) > 0 || (event.remainingSpots || 0) > 0) && (
        <div className="flex gap-1 flex-wrap">
          <ParticipationDialog data={event} />
          <SimpleDialog
            title="Účastníci"
            button={<Button>Účastníci ({event.attendeeUsers?.nodes?.length})</Button>}
          >
            {!!event.attendeeUsers?.nodes?.length && <u>Členové</u>}
            {(event.attendeeUsers?.nodes ?? []).map((x) => (
              <div>
                {x.user?.uJmeno} {x.user?.uPrijmeni}
              </div>
            ))}
            {!!event.attendeeExternals?.nodes?.length && <u>Externí</u>}
            {(event.attendeeExternals?.nodes ?? []).map((x) => (
              <div>
                {x.firstName} {x.lastName}
              </div>
            ))}
          </SimpleDialog>
        </div>
      )}
    </Card>
  );
};
