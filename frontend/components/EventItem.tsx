import * as React from 'react';
import { HtmlView } from 'components/HtmlView';
import { Card } from 'components/Card';
import { fullDateFormatter } from 'lib/format-date';
import { Button } from './Button';
import { SimpleDialog } from './Dialog';
import { MyEventFragment } from 'lib/graphql/Event';
import { ParticipationForm } from './ParticipationForm';

export const EventItem = ({ event, expanded: expandedInit = false }: {
  event: MyEventFragment;
  expanded?: boolean;
}) => {
  const [expanded, setExpanded] = React.useState(expandedInit);
  const open = React.useCallback(() => setExpanded(true), []);

  return (
    <Card className="break-inside-avoid">
      <div className="flex flex-col lg:flex-row-reverse lg:order-reverse justify-between">
        {(event.signedUp || event.hasCapacity) ? (
          <SimpleDialog title={event.name} button={<Button>
            {event.signedUp ? "Upravit přihlášku" : "Přihlásit"}
          </Button>}>
            {({ close }) => <ParticipationForm data={event} onSuccess={close} />}
          </SimpleDialog>
        ) : null}

        <div className="text-lg text-stone-600">
          {fullDateFormatter.formatRange(new Date(event.since || ''), new Date(event.until || ''))}
        </div>
      </div>

      <div className="text-4xl text-stone-800">{event.name}</div>

      <div
        className={expanded ? '' : "cursor-pointer"}
        onClick={!expanded ? open : undefined}
      >
        <HtmlView
          className={expanded ? "" : "line-clamp-3 after"}
          content={(event.info || '').replaceAll('\n', '<br/>')}
        />
        {!expanded && (
          <div className="text-red-500 font-bold mt-3">Zobrazit více...</div>
        )}
      </div>
    </Card>
  );
};
