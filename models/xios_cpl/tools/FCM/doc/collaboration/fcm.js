//------------------------------------------------------------------------------
// JS for displaying information of FCM documents.
//------------------------------------------------------------------------------
FCM = {
    // List of heading tags for the contents
    CONTENT_INDEX_OF: {'H2': 0, 'H3': 1, 'H4': 2, 'H5': 3, 'H6': 4},
    // ID in a document, for the content
    ID_OF_CONTENT: 'fcm-content',
    // ID in a document, for the maintenance information
    ID_OF_MAINTENANCE: 'fcm-js-maintenance',
    // ID in a document, for the link to its PDF version
    ID_OF_PDF: 'fcm-js-pdf',
    // ID in a document, for the breadcrumb trail
    ID_OF_TRAIL: 'fcm-js-trail',
    // URL to the log browser of the FCM trunk
    URL_OF_LOG_BROWSER: 'http://fcm1/projects/FCM/intertrac/log:/FCM/trunk/',
    // URL to the FCM team page
    URL_OF_TEAM: null,

    // Making DOM manipulation less painful
    DOM: {
        // The current document
        DOC: document,

        // Adds an array of "nodes" (Node or String) to "node"
        append: function(node, items) {
            if (node == null || items == null) {
                return;
            }
            for (var i = 0; i < items.length; i++) {
                if (items[i] == null) {
                    continue;
                }
                if (items[i].nodeType == null) {
                    node.appendChild(this.DOC.createTextNode(items[i]));
                }
                else {
                    node.appendChild(items[i]);
                }
            }
        },

        // Returns the base name of a URL, relative to current document
        basename: function(url) {
            if (url == null) {
                url = this.DOC.URL;
            }
            return url.substr(this.DOC.URL.lastIndexOf('/') + 1);
        },

        // Creates and returns an element
        create: function(tag, items, attribs) {
            var element = this.DOC.createElement(tag);
            this.append(element, items);
            if (attribs == null) {
                return element;
            }
            for (var i = 0; i < attribs.length; i++) {
                element[attribs[i][0]] = attribs[i][1];
            }
            return element;
        },

        // Shorthand for root[attrib]
        get_attrib: function(root, attrib) {
            return root[attrib];
        },

        // Shorthand for this.DOC.getElementById(id)
        get_by_id: function(id) {
            return this.DOC.getElementById(id);
        },

        // Shorthand for root.getElementsByTagName(name)[i]
        get_by_name: function(root, name, i) {
            var elements = root.getElementsByTagName(name);
            if (i == null) {
                return elements;
            }
            if (elements == null || i >= elements.length) {
                return null;
            }
            return elements[i];
        },

        map: function(action_on, items) {
            var ret = [];
            for (var i = 0; i < items.length; i++) {
                ret = ret.concat(action_on(items[i], i));
            }
            return ret;
        }
    },

    // Creates and returns the content
    create_content: function() {
        var root = $D.get_attrib($D.DOC, 'body');
        var stack = [{content: null, item: null}];
        for (var node = root.firstChild; node != null; node = node.nextSibling) {
            if (
                node.nodeType != 1
                || node.id == null
                || node.id == ''
                || this.CONTENT_INDEX_OF[node.tagName] == null
                || this.CONTENT_INDEX_OF[node.tagName] + 1 > stack.length
            ) {
                continue;
            }
            while (this.CONTENT_INDEX_OF[node.tagName] + 1 < stack.length) {
                stack.shift();
            }
            if (stack[0].content == null) {
                stack[0].content = $D.create('ul');
                if (stack[0].item != null) {
                    $D.append(stack[0].item, [stack[0].content]);
                }
            }
            var item = $D.create(
                'li',
                [$D.create('a', [node.innerHTML], [['href', '#' + node.id]])]
            );
            $D.append(stack[0].content, [item]);
            stack.unshift({content: null, item: item});
        }
        return [stack[stack.length - 1].content];
    },

    // Creates and returns the maintenance information for "path"
    create_info_for_maintenance: function(path) {
        if (this.URL_OF_TEAM == null) {
            return [];
        }
        return [
            'Maintained by: ',
            $D.create('a', ['FCM team'], [['href', FCM.URL_OF_TEAM]]),
            '. History: ',
            $D.create(
                'a',
                ['log'],
                [['href', this.URL_OF_LOG_BROWSER + path + $D.basename()]]
            ),
            '.'
        ];
    },

    // Creates and returns the PDF link for "name"
    create_info_for_pdf: function(name) {
        return [
            'For printing, please use the ',
            $D.create('a', ['PDF'], [['href', 'fcm-' + name + '.pdf']]),
            ' version of the document.'
        ];
    },

    // Creates a breadcrumb trail from a list of [[href, text], ...]
    create_trail: function(items) {
        if (items == null) {
            return [];
        }
        return [].concat(
            $D.map(
                function(item, i) {
                    return [].concat(
                        (i != 0 ? ' > ' : []),
                        $D.create('a', [item[1]], [['href', item[0]]])
                    );
                },
                items
            ),
            ' > ',
            $D.get_by_name($D.DOC, 'title', 0).innerHTML
        );
    },

    // A simple facade for doing the onload tasks
    load: function(maintenance_path, pdf_name, trail_list) {
        var tasks = [
            {
                id: this.ID_OF_CONTENT,
                task: function() {
                    return FCM.create_content();
                }
            },
            {
                id: this.ID_OF_MAINTENANCE,
                task: function() {
                    return FCM.create_info_for_maintenance(maintenance_path);
                }
            },
            {
                id: this.ID_OF_PDF,
                task: function() {
                    return FCM.create_info_for_pdf(pdf_name);
                }
            },
            {
                id: this.ID_OF_TRAIL,
                task: function() {
                    return FCM.create_trail(trail_list);
                }
            }
        ];
        for (var i = 0; i < tasks.length; i++) {
            var node = $D.get_by_id(tasks[i].id);
            if (node == null) {
                continue;
            }
            $D.append(node, tasks[i].task());
        }
    }
};
$D = FCM.DOM;
