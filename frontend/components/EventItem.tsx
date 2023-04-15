import * as React from 'react';
import { Card } from 'components/Card';
import { fullDateFormatter } from 'lib/format-date';
import { Button } from './Button';
import { SimpleDialog } from './Dialog';
import { MyEventFragment } from 'lib/graphql/Event';
import { ParticipationForm } from './ParticipationForm';
import { RichTextView } from './RichTextView';
import classNames from 'classnames';

export const EventItem = ({
  event,
  expanded: expandedInit = false,
}: {
  event: MyEventFragment;
  expanded?: boolean;
}) => {
  const [expanded, setExpanded] = React.useState(expandedInit);
  const open = React.useCallback(() => setExpanded(true), []);

  return (
    <Card className="break-inside-avoid odd:bg-white even:bg-red-100/20">
      <div className="flex justify-between flex-wrap text-stone-600">
        <div>
          {fullDateFormatter.formatRange(
            new Date(event.aOd || ''),
            new Date(event.aDo || ''),
          )}
        </div>
        <div>
          Zbývá {event.freeSlots} míst z {event.aKapacita}
        </div>
      </div>
      <div className="text-2xl text-stone-900">{event.aJmeno}</div>
      <div className="text-stone-600">{event.aKde}</div>

      <div
        className={classNames('mb-2', !expanded && 'cursor-pointer')}
        onClick={!expanded ? open : undefined}
      >
        <RichTextView value={event.summary} />

        {expanded ? (
          <RichTextView value={(event.aInfo || '').replaceAll('\n', '<br/>')} />
        ) : (
          <div className="text-red-500 font-bold mt-3">Zobrazit více...</div>
        )}
      </div>
      {event.signedUp || event.hasCapacity ? (
        <div>
          <SimpleDialog
            title={event.aJmeno}
            button={<Button>{event.signedUp ? 'Upravit přihlášku' : 'Přihlásit'}</Button>}
          >
            {({ close }) => <ParticipationForm data={event} onSuccess={close} />}
          </SimpleDialog>
        </div>
      ) : null}
    </Card>
  );
};
