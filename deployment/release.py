# -*- coding: utf-8 -*-
    """
  Free-mtrix - Free cultural selection and social behavior experiments.
  Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.  
"""

# for linux only

from subprocess import check_output, CalledProcessError, STDOUT
import os
import shutil
import ntpath

from glob import glob

def get_tag_commit():
    """
    origial script from here:
    https://github.com/pupil-labs/pupil/blob/master/pupil_src/shared_modules/version_utils.py

    returns string: 'tag'-'commits since tag'-'7 digit commit id'
    """
    try:
        return check_output(['git', 'describe', '--tags'],stderr=STDOUT,cwd=os.path.dirname(os.path.abspath(__file__)))
    except CalledProcessError as e:
        logger.error('Error calling git: "{}" \n output: "{}"'.format(e,e.output))
        return None
    except OSError as e:
        logger.error('Could not call git, is it installed? error msg: "{}"'.format(e))
        return None


def get_files(src_directory):
    target_filters = ['*.html']
    glob_lists = [glob(os.path.join(src_directory, tf)) for tf in target_filters]
    return [sorted(glob_list) for glob_list in glob_lists][0]

def compress_folder(src_dir, dst_filename):
    shutil.make_archive(dst_filename, 'zip', src_dir)

def delete_folder(dir_name):
    shutil.rmtree(dir_name)

def copy_folder(src, dst):
    shutil.copytree(src, dst)

def copy_file(src, dst):
    shutil.copyfile(src,dst)

RELEASES_FOLDER = 'deployment'
DOCS_FOLDER = 'docs'
TEST_FOLDER = 'tests'
SRC_MEDIA_FOLDER = os.path.join('experiment_runner', 'media')
DST_MEDIA_FOLDER = 'media'
if __name__ == "__main__":
    root_path = os.path.dirname(os.path.abspath(__file__))
    root_path = os.path.dirname(root_path)
    print(root_path)

    runner_src_win32_libraries = [
        os.path.join(root_path, 'dependencies/libzmq-builds/windows/32/libzmq.dll')
    ]

    runner_src_win64_libraries = [
        os.path.join(root_path, 'dependencies/libzmq-builds/windows/64/libzmq.dll')
    ]

    targets = {
        # 'experiment_runner/experiment_runner_windows_64bits': runner_src_win64_libraries,
        'experiment_runner/experiment_runner_windows_32bits': runner_src_win32_libraries,
        # 'experiment_designer/experiment_designer_windows_32bits': [None],
        # 'experiment_designer/experiment_designer_windows_64bits' : [None],
        # 'experiment_runner/experiment_runner_linux_64bits': [None],
        # 'experiment_designer/experiment_designer_linux_64bits': [None]
    }

    for build_name, libraries in targets.items(): 
        # use build name as root for each release
        releases = os.path.join(root_path, RELEASES_FOLDER)
        build_basename = os.path.basename(build_name)
        destination = os.path.join(releases, build_basename) 
        if os.path.exists(destination):
            delete_folder(destination)
            os.makedirs(destination)
            os.makedirs(os.path.join(destination, DST_MEDIA_FOLDER))
        else:
            os.makedirs(destination)
            os.makedirs(os.path.join(destination, DST_MEDIA_FOLDER))

        # copy build.exe as app_name.exe
        APPLICATION_FILENAME = os.path.dirname(build_name)
        if 'windows' in build_name:
            src = build_name+'.exe'
            dst = os.path.join(destination, APPLICATION_FILENAME+'.exe')

        if 'linux' in build_name:
            src = build_name
            dst = os.path.join(destination, APPLICATION_FILENAME)

        src = os.path.join(root_path, src)
        copy_file(src, dst)

        # copy dlls
        for src_library in libraries:
            if src_library:
                dst_library = os.path.join(destination, os.path.basename(src_library))
                copy_file(src_library, dst_library)

        # copy doc as documentation
        src_examples = os.path.join(root_path, DOCS_FOLDER)
        dst_examples = os.path.join(destination, 'Documentation')
        copy_folder(src_examples, dst_examples)

        # copy configuration file
        src_file = os.path.join(root_path, TEST_FOLDER)
        src_file = os.path.join(src_file, 'TGameControl')
        filename = 'Experiment1-ABAB.ini'
        src = os.path.join(src_file, filename)
        dst = os.path.join(destination, filename)
        copy_file(src, dst)
        filename = 'Experiment1-BABA.ini'
        src = os.path.join(src_file, filename)
        dst = os.path.join(destination, filename)
        copy_file(src, dst)

        # copy relevant media files
        src_file = os.path.join(root_path, SRC_MEDIA_FOLDER)
        src_files = get_files(src_file)
        dest_folder = os.path.join(destination, DST_MEDIA_FOLDER)
        for src in src_files:
            dst = os.path.join(dest_folder, ntpath.basename(src))
            copy_file(src, dst)
            
        # copy test program
        src_file = os.path.join(root_path, TEST_FOLDER)
        src_file = os.path.join(src_file, 'TGameControl')


        if 'windows' in build_name:
            src_file = os.path.join(src_file,'tgamecontrol_test.exe')
            dst_file = os.path.join(destination, 'tgamecontrol_test.exe')
        copy_file(src_file, dst_file)
        

        # zip release destination 
        tag = get_tag_commit()
        if tag:
            tag = get_tag_commit()
            tag = tag[:-1]
            dst_filename = os.path.join(releases, build_basename+'_'+tag.decode('utf-8'))
        else:
            dst_filename = os.path.join(releases, build_basename)
        compress_folder(destination, dst_filename)

        print(build_basename+' released.')
