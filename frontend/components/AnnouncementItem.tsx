import classNames from 'classnames';
import { usePermissions } from 'lib/data/use-permissions';
import { fullDateFormatter } from 'lib/format-date';
import { AnnouncementFragment } from 'lib/graphql/Announcement';
import React from 'react';
import { Card } from './Card';
import { CohortColorBoxes } from './CohortColorBox';
import { Dropdown } from './Dropdown';
import { RichTextView } from './RichTextView';
import * as announcement from 'lib/entities/announcement';

export const AnnouncementItem = ({ item }: { item: AnnouncementFragment }) => {
  const [expanded, setExpanded] = React.useState(false);
  const open = React.useCallback(() => setExpanded(true), []);
  const perms = usePermissions();

  return (
    <Card
      onClick={expanded ? undefined : open}
      className={classNames('group', !expanded && 'cursor-pointer')}
    >
      {perms.canEditAnnouncement(item) && (
        <Dropdown
          className="absolute right-1 top-2"
          align="end"
          options={announcement.getMenu(item)}
        />
      )}

      <div className="text-stone-500 text-sm flex flex-wrap items-baseline gap-4">
        <div>
          {[
            fullDateFormatter.format(new Date(item.upTimestampAdd)),
            item.userByUpKdo &&
              `${item.userByUpKdo?.uJmeno} ${item.userByUpKdo?.uPrijmeni}`,
          ]
            .filter(Boolean)
            .join(', ')}
        </div>
        <CohortColorBoxes
          items={item.upozorneniSkupiniesByUpsIdRodic?.nodes.map(
            (x) => x.skupinyByUpsIdSkupina,
          )}
        />
      </div>
      <h2 className="text-lg font-bold mb-4">{item.upNadpis}</h2>

      <RichTextView
        className={expanded ? '' : 'line-clamp-3 after'}
        value={item.upText}
      />
      {!expanded && <div className="text-primary font-bold mt-3">Zobrazit více...</div>}
    </Card>
  );
};
