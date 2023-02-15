import classNames from "classnames";
import { fullDateFormatter } from "lib/format-date";
import { AnnouncementFragment } from "lib/graphql/Announcement";
import React from "react";
import { Card } from "./Card";
import { RichTextView } from "./RichTextView";

const noop = () => { };

export const AnnouncementItem = ({ item }: { item: AnnouncementFragment }) => {
  const [expanded, setExpanded] = React.useState(false);
  const open = React.useCallback(() => setExpanded(true), []);

  return (
    <Card onClick={expanded ? noop : open} className={classNames(
      "mb-4 odd:bg-white even:bg-red-100/30",
      !expanded && 'cursor-pointer',
    )}>
      <h2 className="text-2xl">{item.upNadpis}</h2>
      <div className="text-stone-500 flex flex-wrap items-baseline gap-4 mb-4">
        <div>
          {[
            fullDateFormatter.format(new Date(item.upTimestampAdd)),
            item.userByUpKdo && `${item.userByUpKdo?.uJmeno} ${item.userByUpKdo?.uPrijmeni}`,
          ].filter(Boolean).join(', ')}
        </div>
        {item.upozorneniSkupiniesByUpsIdRodic?.nodes?.length <= 0 ? null : (
          <div className="flex gap-1 border border-gray-200">
            {item.upozorneniSkupiniesByUpsIdRodic.nodes.map((g) =>
              <div className="w-3 h-3"
                key={g.skupinyByUpsIdSkupina?.sColorRgb}
                title={g.skupinyByUpsIdSkupina?.sName}
                style={{ backgroundColor: g.skupinyByUpsIdSkupina?.sColorRgb }}
              />
            )}
          </div>
        )}
      </div>

      <RichTextView className={expanded ? "" : "line-clamp-3 after"} value={item.upText} />
      {!expanded && (
        <div className="text-primary font-bold mt-3">Zobrazit více...</div>
      )}
    </Card>
  );
};
