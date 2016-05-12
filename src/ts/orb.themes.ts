/* global module */
/*jshint eqnull: true*/

'use strict';

export class ThemeManager {

    public themes = {
        red: '#C72C48',
        blue: '#5bc0de',
        green: '#3fb618',
        orange: '#df691a',
        flower: '#A74AC7',
        gray: '#808080',
        black: '#000000',
        white: '#FFFFFF'
    };

    public currentTheme = 'blue';

     isBootstrap() {
        return this.currentTheme === 'bootstrap';
    }

    current(newTheme) {
        if (newTheme) {
            this.currentTheme = this.validateTheme(newTheme);
        }

        return this.currentTheme;
    };

    validateTheme(themeName) {
        themeName = (themeName || '').toString().trim();
        if (!this.themes[themeName] && themeName !== 'bootstrap') {
            return 'blue';
        } else {
            return themeName;
        }
    };

    getPivotClasses() {
        return {
            container: 'orb-container orb-' + this.currentTheme,
            table: 'orb' + (this.isBootstrap() ? ' table' : '')
        };
    };

    getButtonClasses() {
        return {
            pivotButton: 'fld-btn' + (this.isBootstrap() ? ' btn btn-default btn-xs' : ''),
            orbButton: 'orb-btn' + (this.isBootstrap() ? ' btn btn-default btn-xs' : ''),
            scrollBar: this.isBootstrap() ? ' btn btn-default btn-xs' : ''
        };
    };

    getFilterClasses() {
        return {
            container: 'orb-' + this.currentTheme + ' orb fltr-cntnr'
        };
    };

    getGridClasses() {
        return {
            table: this.isBootstrap() ? 'table table-condensed' : 'orb-table'
        };
    };

    getDialogClasses(visible) {
        var classes = {
            overlay: 'orb-overlay orb-overlay-' + (visible ? 'visible' : 'hidden') + ' orb-' + this.currentTheme,
            dialog: 'orb-dialog',
            content: '',
            header: 'orb-dialog-header',
            title: '',
            body: 'orb-dialog-body'
        };

        if (this.isBootstrap()) {
            classes.overlay += ' modal';
            classes.dialog += ' modal-dialog';
            classes.content = 'modal-content';
            classes.header += ' modal-header';
            classes.title = 'modal-title';
            classes.body += ' modal-body';
        }
        return classes;
    };

    public utils = {
        hexToRgb: function(hex) {
            var result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
            return result ? {
                r: parseInt(result[1], 16),
                g: parseInt(result[2], 16),
                b: parseInt(result[3], 16)
            } : null;
        },
        rgbaToHex: function(rgba) {
            var matches = rgba.match(/rgba\((\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+(?:\.\d+)?)\s*\)/);
            if (matches) {
                var alpha = parseFloat(matches[4]);
                return '#' +
                    this.applyAlphaAndToHex(matches[1], alpha) +
                    this.applyAlphaAndToHex(matches[2], alpha) +
                    this.applyAlphaAndToHex(matches[3], alpha);
            }
            return null;
        },
        rgbaToHexA: function(rgba) {
            var matches = rgba.match(/rgba\((\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+(?:\.\d+)?)\s*\)/);
            if (matches) {
                var alpha = parseFloat(matches[4]);
                return '#' +
                    this.applyAlphaAndToHex(0, alpha) +
                    this.applyAlphaAndToHex(matches[1], 1) +
                    this.applyAlphaAndToHex(matches[2], 1) +
                    this.applyAlphaAndToHex(matches[3], 1);
            }
            return null;
        },
        applyAlphaAndToHex: function(value, alpha) {
            return (Math.floor(alpha * parseInt(value) + (1 - alpha) * 255) + 256).toString(16).substr(1, 2);
        },
        fadeoutColor: function(color, alpha) {
            color = this.hexToRgb(color);
            return '#' +
                this.applyAlphaAndToHex(color.r, alpha) +
                this.applyAlphaAndToHex(color.g, alpha) +
                this.applyAlphaAndToHex(color.b, alpha);
        }
    };

};
