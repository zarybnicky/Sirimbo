import * as React from 'react';
import { Card } from 'components/Card';
import { fullDateFormatter } from 'lib/format-date';
import { Button } from './Button';
import { SimpleDialog } from './Dialog';
import { MyEventFragment } from 'lib/graphql/Event';
import { ParticipationForm } from './ParticipationForm';
import { RichTextView } from './RichTextView';

export const EventItem = ({ event, expanded: expandedInit = false }: {
  event: MyEventFragment;
  expanded?: boolean;
}) => {
  const [expanded, setExpanded] = React.useState(expandedInit);
  const open = React.useCallback(() => setExpanded(true), []);

  return (
    <Card className="break-inside-avoid">
      <div className="flex flex-col-reverse lg:flex-row justify-between">
        <div className="text-lg text-stone-600">
          {fullDateFormatter.formatRange(new Date(event.aOd || ''), new Date(event.aDo || ''))}
        </div>

        {(event.signedUp || event.hasCapacity) ? <div>
          <SimpleDialog title={event.aJmeno} button={<Button>
            {event.signedUp ? "Upravit přihlášku" : "Přihlásit"}
          </Button>}>
            {({ close }) => <ParticipationForm data={event} onSuccess={close} />}
          </SimpleDialog>
        </div> : null}
      </div>

      <div className="text-2xl text-stone-800">{event.aJmeno}</div>
      <div className="text-xl text-stone-500">{event.aKde}</div>
      <div className="text-stone-500">Zbývá {event.freeSlots} míst z {event.aKapacita}</div>

      <div
        className={expanded ? '' : "cursor-pointer"}
        onClick={!expanded ? open : undefined}
      >
        <RichTextView value={event.summary} />

        {expanded ? (
          <RichTextView value={(event.aInfo || '').replaceAll('\n', '<br/>')} />
        ) : (
          <div className="text-red-500 font-bold mt-3">Zobrazit více...</div>
        )}
      </div>
    </Card>
  );
};
